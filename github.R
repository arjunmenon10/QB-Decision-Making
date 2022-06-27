library(tidyverse)
library(caret)
library(tidymodels)

WR_open <- read_csv("all_coverage_offenseQBHelp.csv")
WR_open1920 <- read_csv("all_coverage_offenseQBHelp1920.csv")

WR_all_target <- rbind(WR_open, WR_open1920) |> 
  filter(target == 1) |> 
  select(route_name,open,player_name, game_id, play_id,route_depth)

passingother <- read_csv("passingother21.csv")
passingother1920 <- read_csv("passingother1920.csv")

passingotherall <- rbind(passingother, passingother1920)

passingotherall <- passingotherall %>% 
  select(game_id, play_id, target_width, rpo, pass_rusher_count, target_yard_line)

passing_pbp <- read_csv("passing_feed (49).csv")
passing_pbp2 <- read_csv("passing_feed (50).csv")
passing_pbp <- rbind(passing_pbp, passing_pbp2)

epa_data <- read_csv("epa_data.csv")

epa_main <- epa_data |> 
  select(game_id, play_id, EP, EPA, EPn1, seconds_left_in_quarter, yards_to_go)

coverage21 <- read_csv("shiny_cov21.csv")
coverage20 <- read_csv("shiny_cov20.csv")
coverage19 <- read_csv("shiny_cov19.csv")
coverage_all <- rbind(coverage19, coverage20, coverage21)

passing_pbp <- left_join(passing_pbp, epa_main, by = c('game_id', 'play_id'))

PFF_DEF <- function(coverage_all,
                    league = "nfl",
                    season_min = 2019, season_max = 2021,
                    week_min = 1, week_max = 32,
                    THRESH = 100, THRESH_QUANT = 1/3, alpha = 0.075) {
  
  decay <- function(alpha, x) {exp(-alpha*x)}
  thresh <- function(x, y, THRESH, mu) {
    return((x/THRESH)*y + (1 - (x/THRESH))*mu)
  }
  
  if (league == "nfl") {
    MINOR_WEIGHTS <- data.frame( ###Eventually need to read this in from somewhere
      PFF = 1/5,
      neg = 1/5)
    
    coverage_all <- coverage_all %>%
      mutate(PFF_grade = MINOR_WEIGHTS$PFF*grade_OE,
             negative_rate = MINOR_WEIGHTS$neg*0,
             snap = 1)
  }
  
  ### Folding Up Data, and Regressing By Age of Data ###########
  if (league == "nfl") {
    week <- 17
  } else if (league == "ncaa") {
    week <- 14
  }
  
  df <- data.frame()
  for (i in season_min:season_max) {
    if (league == "ncaa") {
      week_set <- 0:20
    } else if (league == "nfl" & i <= 2020) {
      week_set <- c(1:17, 28:30, 32)
    } else if (league == "nfl" & i >= 2021) {
      week_set <- c(1:18, 28:30, 32)
    }
    
    for (j in week_set) {
      WEEK_REG <- case_when(league == "ncaa" & j >= week ~ week, 
                            league == "nfl" & i <= 2020 & j >= week ~ week, 
                            league == "nfl" & i >= 2021 & j >= (week + 1) ~ week + 1, 
                            TRUE ~ j + 0)
      
      CURRENT_WEEK <- case_when(league == "ncaa" ~ WEEK_REG + week*(i - season_min), 
                                league == "nfl" & i <= 2020 ~ WEEK_REG + week*(i - season_min), 
                                league == "nfl" & i >= 2021 ~ WEEK_REG + (week + 1)*(i - 2021) + week*(2021 - season_min))
      
      temp <- coverage_all %>% filter(season %in% c(i - 1, i - 2) | (season == i & week < j)) %>%
        group_by(player_name) %>%
        summarize(PFF_grade = sum(decay(alpha, CURRENT_WEEK - week)*grade_OE),
                  negative_rate = sum(decay(alpha, CURRENT_WEEK - week)*0),
                  PFF_snaps = sum(decay(alpha, CURRENT_WEEK - week)*snap)) %>%
        mutate(season = i, week = j) %>%
        as.data.frame()
      
      df <- rbind(df, temp)
    }
  }
  
  PFF_Thresh <- quantile(filter(df, PFF_snaps >= THRESH)$PFF_grade/filter(df, PFF_snaps >= THRESH)$PFF_snaps, THRESH_QUANT)
  negative_Thresh <- quantile(filter(df, PFF_snaps >= THRESH)$negative_rate/filter(df, PFF_snaps >= THRESH)$PFF_snaps, 1-THRESH_QUANT)
  
  df <- df %>%
    mutate(PFF_grade = ifelse(PFF_snaps >= THRESH, PFF_grade/PFF_snaps,
                              thresh(PFF_snaps, PFF_grade/PFF_snaps, THRESH, PFF_Thresh)),
           negative_rate = ifelse(PFF_snaps >= THRESH, negative_rate/PFF_snaps,
                                  thresh(PFF_snaps, negative_rate/PFF_snaps, THRESH, PFF_Thresh))) %>%
    mutate(negative_rate = -negative_rate)
  
  return(df)
}

DEF_check<-PFF_DEF(coverage_all)


coverage_all <- coverage_all |> 
  select(player_name, grade_OE, game_id, play_id, EPA)

passing_join <- left_join(passing_pbp, coverage_all, by = c('game_id', 'play_id',
                                                            'coverage_name' = 'player_name',
                                                            'EPA')) |> 
  left_join(WR_all_target, by = c('game_id', 'play_id')) |> 
  left_join(passingotherall, by = c('game_id', 'play_id')) |> 
  group_by(game_id, play_id) |> distinct() |> 
  filter(!is.na(coverage_name)) |> 
  filter(!is.na(grade_OE))

passing_join2 <- left_join(passing_join, DEF_check, by = c('coverage_name' = 'player_name',
                                                           'season', 'week'))

passing_join2 <- passing_join2 %>%
  mutate(seconds_left_in_half = case_when(
    quarter == 1 ~ as.integer(seconds_left_in_quarter + 900),
    quarter == 2 ~ as.integer(seconds_left_in_quarter),
    quarter == 3 ~ as.integer(seconds_left_in_quarter + 900),
    quarter == 4 ~ as.integer(seconds_left_in_quarter),
    quarter == 5 ~ as.integer(seconds_left_in_quarter)
  )) %>%
  select(-seconds_left_in_quarter)


passing_join2$completion[is.na(passing_join2$completion)] <- 0

passing_join2<-passing_join2%>%
  mutate(coverage_rate=negative_rate+PFF_grade)

WR_all_target1 <- left_join(passing_join2,WR_all_target,by=c("game_id","play_id","player_name","route_depth","open"))


WR_all_target1<-WR_all_target1%>%
  mutate(WR_win=case_when(
    open=="Open"~1,
    TRUE~0
  ))


passing_model_data <- WR_all_target1 %>%
  select(-route_name.x) |> 
  rename(route_name = 'route_name.y') |> 
  ungroup() |> 
  dplyr::select(completion, season, distance, down, seconds_left_in_half, yards_to_go,
                play_action, shotgun, rpo, screen, pass_box, route_name, WR_win,coverage_rate,
                route_depth, pass_rusher_count, dropback_depth,
                qb_moved,hurry,pressure) %>%
  mutate(label = as.factor(completion)) %>%
  select(-completion)

passing_model_data <- passing_model_data |> 
  mutate(
    route_name = case_when(
      route_name ==     'Post-Corner-Post' ~ "Post",
      route_name ==     'Out-Skinny Post' ~ 'Post',
      route_name ==     'Out-Post' ~ 'Post',
      route_name ==     'Corner-Skinny Post' ~ 'Post',
      route_name ==     'Slant-Post' ~ 'Post',
      route_name ==     'Sluggo-Comeback' ~ 'Comeback',
      route_name ==     'Flat & Up, Comeback' ~ 'Comeback',
      route_name ==     'Hitch & Go, Comeback' ~ 'Comeback',
      route_name ==     'Out & Up, Comeback' ~ 'Comeback',
      route_name ==     'Post-In' ~ 'In',
      route_name ==     'Sluggo-In' ~ 'In',
      route_name ==     'Whip-Out, In' ~ 'In',
      route_name ==     'Hitch & Go, Out' ~ 'Out',
      route_name ==     'Flat & Up, Out' ~ 'Out',
      route_name ==     'In & Out' ~ 'Out',
      route_name ==     'Sluggo-Out' ~ 'Out',
      route_name ==     'Wheel-Out' ~ 'Out',
      route_name ==     'Whip-In, Out' ~ 'Out',
      TRUE ~ route_name
    )
  )

passing_model_data <- passing_model_data %>% 
  mutate(
    route_name = case_when(
      route_name ==     'Post-Corner-Seam' ~ 'Seam',
      route_name ==     'In-Seam' ~ 'Seam',
      route_name ==     'Hitch & Go Seam' ~ 'Seam',
      route_name ==     'Post-Seam' ~ 'Seam',
      route_name ==     'Sluggo Seam' ~ 'Seam',
      route_name ==     'Corner-Seam' ~ 'Seam',
      route_name ==     'Cross-Seam' ~ 'Seam',
      route_name ==     'Out & Up Seam' ~ 'Seam',
      route_name ==     'Back Shoulder Seam' ~ 'Seam',
      route_name ==     'Hitch-Corner' ~ 'Corner',
      route_name ==     'In-Corner' ~ 'Corner',
      route_name ==     'Wheel-Corner' ~ 'Corner',
      route_name ==     'Slant-Corner' ~ 'Corner',
      route_name ==     'Back Shoulder Out & Up' ~ 'Out & Up',
      route_name ==     'HB whip right' ~ 'HB Whip',
      route_name ==     'HB whip left' ~ 'HB Whip',
      route_name ==     'Sluggo-Hitch' ~ 'Hitch',
      route_name ==     'Wheel-Hitch' ~ 'Hitch',
      route_name ==     'Corner-Post, Hitch' ~ 'Hitch',
      route_name ==     'Flat & Up, Hitch' ~ 'Hitch',
      route_name ==     'Out & Up, Hitch' ~ 'Hitch',
      route_name ==     'Hitch & Go, Hitch' ~ 'Hitch',
      route_name ==     'Flat-Hitch' ~ 'Hitch',
      TRUE ~ route_name
    )
  )

passing_model_data <- passing_model_data %>% 
  mutate(
    route_name = case_when(
      route_name == 'Flat & Up, In' ~ 'In',
      route_name == 'Wheel-Back' ~ 'Wheel',
      route_name == 'Wheel-In' ~ 'Wheel',
      route_name == 'Back Shoulder Hitch & Go' ~ 'Hitch & Go',
      route_name == 'Hitch & Go, In' ~ 'In',
      route_name == 'Flat & Post' ~ 'Post',
      route_name == 'Hide drag' ~ 'Drag',
      route_name == 'Hitch-Post' ~ 'Post',
      route_name == 'Post-Corner, Sit' ~ 'Sit',
      route_name == 'Back Shoulder Flat & Up' ~ 'Flat & Up',
      route_name == 'Back Shoulder Wheel' ~ 'Wheel',
      route_name == 'In & Up' ~ 'Nod Go',
      route_name == 'Arrow Stop' ~ 'Arrow',
      route_name == 'Jet wheel' ~ 'Wheel',
      route_name == 'Ghost Flare' ~ 'Flare',
      route_name == 'Post-Go' ~ 'Go',
      route_name == 'Nod Go' ~ 'Go',
      TRUE ~ route_name
    )
  )

passing_model_data <- passing_model_data %>% 
  mutate(
    route_name = case_when(
      route_name == 'Slant-Hitch' ~ 'Hitch',
      route_name == 'Corner-Go' ~ 'Go',
      route_name == 'Whip-Out' ~ 'Out',
      route_name == 'Corner-Sit' ~ 'Corner',
      route_name == 'Out-Hitch' ~ 'Hitch',
      route_name == 'Tunnel' ~ 'WR Screen',
      route_name == 'Hitch-Out' ~ 'Out',
      route_name == 'Numbers go' ~ 'Go',
      route_name == 'Back Shoulder Go' ~ 'Go',
      route_name == 'Corner-Post' ~ 'Post',
      route_name == 'Nod go' ~ 'Go',
      route_name == 'Post-Corner' ~ 'Corner',
      route_name == 'Block run through' ~ 'Hitch',
      route_name == 'Post-Out' ~ 'Out',
      route_name == 'In-Hitch' ~ 'Hitch',
      route_name == 'Ghost' ~ 'Go',
      route_name == 'Out-Sit' ~ 'Hitch',
      route_name == 'HB angle left' ~ 'Angle',
      route_name == 'HB angle right' ~ 'Angle',
      route_name == 'Skinny Post' ~ 'Post',
      route_name == 'Shake' ~ 'Corner',
      route_name == 'Drag-Wheel' ~ 'Wheel',
      route_name == 'Whip-In' ~ 'In',
      route_name == 'Stutter go' ~ 'Go',
      route_name == 'Hitch & Go' ~ 'Go',
      route_name == 'Hitch-In' ~ 'In',
      route_name == 'Hot' ~ 'Slant',
      route_name == 'Flat-up' ~ 'Go',
      route_name == 'HB Whip' ~ 'Angle',
      route_name == 'Arrow' ~ 'Angle',
      route_name == 'Nod Go' ~ 'Go',
      route_name == 'Out & In' ~ 'In',
      TRUE ~ route_name
    ))

view(unique(passing_model_data$route_name))

passing_model_data$shotgun[is.na(passing_model_data$shotgun)] <- 0
passing_model_data$hurry[is.na(passing_model_data$hurry)] <- 0
passing_model_data$pressure[is.na(passing_model_data$pressure)] <- 0
passing_model_data$route_depth[is.na(passing_model_data$route_depth)] <- 0
passing_model_data$target_width[is.na(passing_model_data$target_width)] <- 0
passing_model_data$pass_rusher_count[is.na(passing_model_data$pass_rusher_count)] <- 0
passing_model_data$target_yard_line[is.na(passing_model_data$target_yard_line)] <- 0
passing_model_data$route_name[is.na(passing_model_data$route_name)] <- "Other"



passing_model_data$shotgun <- as.factor(passing_model_data$shotgun)
passing_model_data$route_name <- as.factor(passing_model_data$route_name)

passing_model_data <- mutate_all(passing_model_data, ~replace(., is.na(.), 0))



#passing_model_data<-passing_model_data%>%
# filter(!is.na(coverage_rate))

colSums(is.na(passing_model_data))

#passing_join2<-passing_join2 |> 
# filter(!is.na(coverage_rate)) 


set.seed(1994)
tidy_split <- initial_split(passing_model_data)
train_data <- training(tidy_split)
test_data <- testing(tidy_split)



ControlParamteres <- trainControl(method = "cv", number = 5,
                                  savePredictions = TRUE, classProbs = FALSE,
                                  verboseIter = TRUE,
                                  allowParallel = TRUE)

parametersGrid <-  expand.grid(eta = c(0.1), colsample_bytree = c(0.25, 0.75),
                               max_depth = c(2,4,8), nrounds = c(1000),
                               gamma = 1, min_child_weight = 1, subsample = 1)

fit5 <- train(label~.,data=train_data, method = "xgbTree", trControl = ControlParamteres,
              tuneGrid = parametersGrid)

#save(fit5, file = "CPOEQB_XGBoost.rda")
#load("~/CPOEQB_XGBoost.rda")


varImp(fit5,scale=FALSE)
xgboost::xgb.importance(fit5$finalModel$feature_names,model=fit5$finalModel)


CP_model_data_fitted <- as.data.frame(predict(fit5, newdata = passing_model_data,
                                              type = 'prob'))



CP_model_data_fitted <- CP_model_data_fitted |> 
  rename(zero = '0',
         one = '1')

CP_model_data_fitted <- CP_model_data_fitted |> 
  mutate(
    CP_probs = (zero*0 + one*1)
  ) |> select(CP_probs)


CP_model_data_fitted |> 
  ungroup() |> summarise(mean = mean(CP_probs))


model_fit_join <- cbind(passing_model_data, CP_model_data_fitted)
model_fit_join_values <- model_fit_join |> 
  select(CP_probs)



CPOE <- cbind(WR_all_target1, model_fit_join_values)%>%
  mutate(CPOE=completion-CP_probs)%>%
  mutate(COMP=completion)%>%
  select(-route_name.y)%>%
  rename(route_name=route_name.x)




qb_test<-CPOE%>%
  group_by(passer_name)%>%
  summarise(attempts=n(),
            expected_cp=mean(CP_probs),
            comp_pct=mean(completion),
            CPOE=mean(CPOE))%>%filter(attempts>200)


### APPLY CP MODEL TO ALL ROUTES

WR_all_route <- rbind(WR_open, WR_open1920) |> 
  select(player_name, game_id, play_id, route_depth, open, route_name)

coverage_data_def <- read.csv("RDcovdatadef.csv") |> 
  select(game_id, play_id, player_name, primary_matchup_player_name) |> 
  rename(coverage_name = 'player_name')

WR_all_route <- left_join(WR_all_route, coverage_data_def, by = c('game_id', 'play_id',
                                                                  'player_name' = 'primary_matchup_player_name')) |> 
  group_by(game_id, play_id, player_name) |> distinct()

passing_all_route <- left_join(WR_all_route, passing_pbp, by = c('game_id', 'play_id')) |> 
  select(-coverage_name.y) |> rename(coverage_name = 'coverage_name.x') |> 
  left_join(coverage_all, by = c('game_id', 'play_id','coverage_name' = 'player_name',
                                 'EPA')) |> 
  left_join(passingotherall, by = c('game_id', 'play_id')) |> 
  group_by(game_id, play_id) |> distinct() |> 
  filter(!is.na(coverage_name)) |> 
  filter(!is.na(grade_OE))

passing_all_route <- left_join(passing_all_route, DEF_check, by = c('coverage_name' = 'player_name',
                                                                    'season', 'week'))

passing_all_route <- passing_all_route %>%
  mutate(seconds_left_in_half = case_when(
    quarter == 1 ~ as.integer(seconds_left_in_quarter + 900),
    quarter == 2 ~ as.integer(seconds_left_in_quarter),
    quarter == 3 ~ as.integer(seconds_left_in_quarter + 900),
    quarter == 4 ~ as.integer(seconds_left_in_quarter),
    quarter == 5 ~ as.integer(seconds_left_in_quarter)
  )) %>%
  select(-seconds_left_in_quarter)


passing_all_route$completion[is.na(passing_all_route$completion)] <- 0

passing_all_route<-passing_all_route%>%
  mutate(coverage_rate=negative_rate+PFF_grade)

passing_all_route<-passing_all_route%>%
  mutate(WR_win=case_when(
    open=="Open"~1,
    TRUE~0
  ))

passing_all_route$shotgun[is.na(passing_all_route$shotgun)] <- 0
passing_all_route$hurry[is.na(passing_all_route$hurry)] <- 0
passing_all_route$pressure[is.na(passing_all_route$pressure)] <- 0
passing_all_route$route_name[is.na(passing_all_route$route_name)] <- 0
passing_all_route$target_width[is.na(passing_all_route$target_width)] <- 0
passing_all_route$pass_rusher_count[is.na(passing_all_route$pass_rusher_count)] <- 0
passing_all_route$target_yard_line[is.na(passing_all_route$target_yard_line)] <- 0

passing_all_route$route_name <- as.character(passing_all_route$route_name)

passing_all_route <- passing_all_route |> 
  mutate(
    route_name = case_when(
      route_name == "Flat & Post" ~ "Post",
      route_name == 'Flat & Up, In' ~ 'In',
      route_name == 'Hide drag' ~ 'Drag',
      route_name == 'Hitch & Go, In' ~ 'In',
      route_name == 'Post-Corner, Sit' ~ 'Corner',
      route_name == 'Sit' ~ 'Hitch',
      route_name == 'Wheel-Back' ~ 'Wheel',
      route_name == 'Wheel-In' ~ 'Drag',
      route_name == 'Dig' ~ 'Drag',
      route_name == 'Back & Go' ~ 'Go',
      route_name == 'Back & In' ~ 'In',
      route_name == 'Back Shoulder Sluggo' ~ 'Out & Up',
      route_name == 'Slant-Skinny Post' ~ 'Post',
      TRUE ~ route_name
    )
  )

passing_all_route <- passing_all_route |> 
  mutate(
    route_name = case_when(
      route_name ==     'Post-Corner-Post' ~ "Post",
      route_name ==     'Out-Skinny Post' ~ 'Post',
      route_name ==     'Out-Post' ~ 'Post',
      route_name ==     'Corner-Skinny Post' ~ 'Post',
      route_name ==     'Slant-Post' ~ 'Post',
      route_name ==     'Sluggo-Comeback' ~ 'Comeback',
      route_name ==     'Flat & Up, Comeback' ~ 'Comeback',
      route_name ==     'Hitch & Go, Comeback' ~ 'Comeback',
      route_name ==     'Out & Up, Comeback' ~ 'Comeback',
      route_name ==     'Post-In' ~ 'In',
      route_name ==     'Sluggo-In' ~ 'In',
      route_name ==     'Whip-Out, In' ~ 'In',
      route_name ==     'Hitch & Go, Out' ~ 'Out',
      route_name ==     'Flat & Up, Out' ~ 'Out',
      route_name ==     'In & Out' ~ 'Out',
      route_name ==     'Sluggo-Out' ~ 'Out',
      route_name ==     'Wheel-Out' ~ 'Out',
      route_name ==     'Whip-In, Out' ~ 'Out',
      TRUE ~ route_name
    )
  )

passing_all_route <- passing_all_route %>% 
  mutate(
    route_name = case_when(
      route_name ==     'Post-Corner-Seam' ~ 'Seam',
      route_name ==     'In-Seam' ~ 'Seam',
      route_name ==     'Hitch & Go Seam' ~ 'Seam',
      route_name ==     'Post-Seam' ~ 'Seam',
      route_name ==     'Sluggo Seam' ~ 'Seam',
      route_name ==     'Corner-Seam' ~ 'Seam',
      route_name ==     'Cross-Seam' ~ 'Seam',
      route_name ==     'Out & Up Seam' ~ 'Seam',
      route_name ==     'Back Shoulder Seam' ~ 'Seam',
      route_name ==     'Hitch-Corner' ~ 'Corner',
      route_name ==     'In-Corner' ~ 'Corner',
      route_name ==     'Wheel-Corner' ~ 'Corner',
      route_name ==     'Slant-Corner' ~ 'Corner',
      route_name ==     'Back Shoulder Out & Up' ~ 'Out & Up',
      route_name ==     'HB whip right' ~ 'HB Whip',
      route_name ==     'HB whip left' ~ 'HB Whip',
      route_name ==     'Sluggo-Hitch' ~ 'Hitch',
      route_name ==     'Wheel-Hitch' ~ 'Hitch',
      route_name ==     'Corner-Post, Hitch' ~ 'Hitch',
      route_name ==     'Flat & Up, Hitch' ~ 'Hitch',
      route_name ==     'Out & Up, Hitch' ~ 'Hitch',
      route_name ==     'Hitch & Go, Hitch' ~ 'Hitch',
      route_name ==     'Flat-Hitch' ~ 'Hitch',
      TRUE ~ route_name
    )
  )

passing_all_route <- passing_all_route %>% 
  mutate(
    route_name = case_when(
      route_name == 'Flat & Up, In' ~ 'In',
      route_name == 'Wheel-Back' ~ 'Wheel',
      route_name == 'Wheel-In' ~ 'Wheel',
      route_name == 'Back Shoulder Hitch & Go' ~ 'Hitch & Go',
      route_name == 'Hitch & Go, In' ~ 'In',
      route_name == 'Flat & Post' ~ 'Post',
      route_name == 'Hide drag' ~ 'Drag',
      route_name == 'Hitch-Post' ~ 'Post',
      route_name == 'Post-Corner, Sit' ~ 'Sit',
      route_name == 'Back Shoulder Flat & Up' ~ 'Flat & Up',
      route_name == 'Back Shoulder Wheel' ~ 'Wheel',
      route_name == 'In & Up' ~ 'Nod Go',
      route_name == 'Arrow Stop' ~ 'Arrow',
      route_name == 'Jet wheel' ~ 'Wheel',
      route_name == 'Ghost Flare' ~ 'Flare',
      route_name == 'Post-Go' ~ 'Go',
      route_name == 'Nod Go' ~ 'Go',
      TRUE ~ route_name
    )
  )

passing_all_route <- passing_all_route %>% 
  mutate(
    route_name = case_when(
      route_name == 'Slant-Hitch' ~ 'Hitch',
      route_name == 'Corner-Go' ~ 'Go',
      route_name == 'Whip-Out' ~ 'Out',
      route_name == 'Corner-Sit' ~ 'Corner',
      route_name == 'Out-Hitch' ~ 'Hitch',
      route_name == 'Tunnel' ~ 'WR Screen',
      route_name == 'Hitch-Out' ~ 'Out',
      route_name == 'Numbers go' ~ 'Go',
      route_name == 'Back Shoulder Go' ~ 'Go',
      route_name == 'Corner-Post' ~ 'Post',
      route_name == 'Nod go' ~ 'Go',
      route_name == 'Post-Corner' ~ 'Corner',
      route_name == 'Block run through' ~ 'Hitch',
      route_name == 'Post-Out' ~ 'Out',
      route_name == 'In-Hitch' ~ 'Hitch',
      route_name == 'Ghost' ~ 'Go',
      route_name == 'Out-Sit' ~ 'Hitch',
      route_name == 'HB angle left' ~ 'Angle',
      route_name == 'HB angle right' ~ 'Angle',
      route_name == 'Skinny Post' ~ 'Post',
      route_name == 'Shake' ~ 'Corner',
      route_name == 'Drag-Wheel' ~ 'Wheel',
      route_name == 'Whip-In' ~ 'In',
      route_name == 'Stutter go' ~ 'Go',
      route_name == 'Hitch & Go' ~ 'Go',
      route_name == 'Hitch-In' ~ 'In',
      route_name == 'Hot' ~ 'Slant',
      route_name == 'Flat-up' ~ 'Go',
      route_name == 'HB Whip' ~ 'Angle',
      route_name == 'Arrow' ~ 'Angle',
      route_name == 'Nod Go' ~ 'Go',
      route_name == 'Out & In' ~ 'In',
      TRUE ~ route_name
    ))

passing_all_route$route_name[is.na(passing_all_route$route_name)] <- "Other"

view(unique(passing_all_route$route_name))

passing_all_route$shotgun <- as.factor(passing_all_route$shotgun)
passing_all_route$route_name <- as.factor(passing_all_route$route_name)

passing_all_route$negative_rate[is.na(passing_all_route$negative_rate)] <- 0
passing_all_route$coverage_rate[is.na(passing_all_route$coverage_rate)] <- 0

colSums(is.na(passing_all_route))

passing_all_route$route_name <- as.character(passing_all_route$route_name)

passing_all_route$route_name <- ifelse(passing_all_route$route_name == 0, 'Other', passing_all_route$route_name)

passing_all_route <- mutate_all(passing_all_route, ~replace(., is.na(.), 0))

CP_route_fitted <- as.data.frame(predict(fit5, newdata = passing_all_route,
                                         type = 'prob'))



CP_route_fitted <- CP_route_fitted |> 
  rename(zero = '0',
         one = '1')

CP_route_fitted <- CP_route_fitted |> 
  mutate(
    CP_probs = (zero*0 + one*1)
  ) |> select(CP_probs)


CP_route_fitted |> 
  ungroup() |> summarise(mean = mean(CP_probs))


model_route_join <- cbind(passing_all_route, CP_route_fitted)
model_route_join_values <- model_route_join |> 
  ungroup() |> 
  select(CP_probs)

CPOE_pbp <- cbind(passing_all_route, model_route_join_values) |> 
  mutate(CPOE=completion-CP_probs)%>%
  mutate(COMP=completion)


### BUILD EPA MODEL

EPA_join<- CPOE_pbp

EPA_use_pbp <- CPOE_pbp |>  ungroup() |> 
  select(EPA, season, distance, down, seconds_left_in_half, yards_to_go,
         play_action, shotgun, time_to_throw, qb_moved,
         route_depth, coverage_rate, CP_probs, route_name,
         WR_win) |> 
  rename(label = 'EPA')

EPA_use_pbp$route_name <- as.factor(EPA_use_pbp$route_name)
EPA_use_pbp$season <- as.factor(EPA_use_pbp$season)

EP_model_data <- CPOE_pbp |> 
  filter(player_name == target_name)

EP_model_data <- EP_model_data |> 
  ungroup() |> 
  select(EPA, season, distance, down, seconds_left_in_half, yards_to_go,
         play_action, shotgun, time_to_throw, qb_moved,
         route_depth, coverage_rate, CP_probs, route_name,
         WR_win) |> 
  mutate(label = EPA) |> select(-EPA)

EP_model_data <- EP_model_data %>% 
  select(label, everything())

EP_model_data$route_name <- as.factor(EP_model_data$route_name)
EP_model_data$season <- as.factor(EP_model_data$season)

trsf <- one_hot(as.data.table(EP_model_data))

dim(trsf)
str(trsf)

smp_size <- floor(0.70 * nrow(trsf))
set.seed(123)
ind <- sample(seq_len(nrow(trsf)), size = smp_size)
train <- as.matrix(trsf[ind, ])
test <- as.matrix(trsf[-ind, ])

EPA_model <-
  xgboost(
    data = train[, 2:48],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3,
    eta = .22
  )   

vip(EPA_model)

EPA_onehot <- one_hot(as.data.table(EPA_use_pbp))

EPA_model_data_fitted <- as.data.frame(
  matrix(predict(EPA_model, as.matrix(EPA_onehot %>% select(-label))))
) %>%
  dplyr::rename(pred_EPA = V1)

EPA_use_join <- cbind(EPA_join, EPA_model_data_fitted)

EPA_use_join |> 
  filter(player_name == target_name) |> 
  ggplot(aes(x = EPA, y = pred_EPA))+
  geom_point()+geom_smooth(method = 'lm')

EPA_use_join <- EPA_use_join |> 
  mutate(EPA_CP = pred_EPA*CP_probs) |> 
  group_by(play_id, player_name) |> 
  dplyr::slice(1)

EPA_high <- EPA_use_join |> 
  group_by(play_id) |> 
  arrange(-pred_EPA) |> dplyr::slice(1) |> 
  mutate(high_pred_EPA = 1) |> 
  select(game_id, play_id, pred_EPA, high_pred_EPA)

EPA_decision <- left_join(EPA_use_join, EPA_high, by = c('game_id', 'play_id',
                                                         'pred_EPA'))

EPA_decision$high_pred_EPA[is.na(EPA_decision$high_pred_EPA)] <- 0



QB_decision_making <- EPA_decision |> 
  group_by(passer_name) |> 
  summarise(
    team = first(offense),
    EPA_O = mean(EPA[player_name == target_name & high_pred_EPA == 1], na.rm = T),
    EPA_NO = mean(EPA[player_name == target_name & high_pred_EPA == 0], na.rm = T),
    dropbacks = sum(dropback[player_name == target_name]),
    optimal_decision = sum(high_pred_EPA[player_name == target_name]),
    optimal_decision_perc = optimal_decision/dropbacks
  ) |> 
  filter(dropbacks > 500) |>  
  mutate(team = case_when(
    team == "SD" ~ "LAC",
    team == "BLT" ~ "BAL",
    team == "OAK" ~ "LV",
    team == "HST" ~ "HOU",
    team == "SL" ~ "LA",
    team == "CLV" ~ "CLE", 
    team == "ARZ" ~ "ARI",
    TRUE ~ team
  )) |> 
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

QB_decision_making_szn <- EPA_decision |> 
  group_by(passer_name, season) |> 
  summarise(
    team = first(offense),
    EPA_O = mean(EPA[player_name == target_name & high_pred_EPA == 1], na.rm = T),
    EPA_NO = mean(EPA[player_name == target_name & high_pred_EPA == 0], na.rm = T),
    dropbacks = sum(dropback[player_name == target_name]),
    optimal_decision = sum(high_pred_EPA[player_name == target_name]),
    optimal_decision_perc = optimal_decision/dropbacks
  ) |> 
  filter(dropbacks > 110) |> 
  arrange(season) |> group_by(passer_name) |> mutate(next_opt = lead(optimal_decision_perc))

QB_decision_making_szn |> 
  ungroup() |> 
  select(optimal_decision_perc, next_opt) |> 
  cor(use = 'complete.obs')

QB_EPA <- EPA_decision |> 
  group_by(passer_name) |> 
  summarise(
    EPA_all = mean(EPA, na.rm = T))

QB_decision_making |> 
  ggplot(aes(x = EPA_O, y = EPA_NO))+
  geom_point(aes(size = optimal_decision_perc), fill = QB_decision_making$team_color,
             color = QB_decision_making$team_color2)+
  geom_text_repel(aes(label = passer_name), color = 'black', size = 6)+
  geom_vline(xintercept = mean(QB_decision_making$EPA_O), lty = 'dashed', color = 'red')+
  geom_hline(yintercept = mean(QB_decision_making$EPA_NO), lty = 'dashed', color = 'red')+
  theme_fivethirtyeight()+
  labs(title = "Best quarterbacks when making the optimal decision vs not making the optimal decision",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
       subtitle = glue::glue("2019-2021 | Dot Size = % of times QB makes optimal decision | Optimal when QB throws to player with highest predicted EPA"))+
  theme(axis.title = element_text(size = 16)) + ylab("EPA/Play when not making optimal decision") + xlab("EPA/Play when making optimal decision")+
  theme(axis.text = element_text(size = 14))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 16, hjust = 0.5),
        legend.position = 'none')+
  scale_x_continuous(breaks = scales::pretty_breaks(n=8))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=8))
ggsave('QBoptimal.png', width = 14, height = 10, dpi = "retina")

QB_decision_making |> 
  left_join(QB_EPA, by = 'passer_name') |> 
  ggplot(aes(x = ADOT, y = optimal_decision_perc))+
  geom_point()+geom_smooth(method = 'lm')

EPA_use_join |>  group_by(play_id) |> 
  mutate(routes = n()) |> 
  group_by(routes) |> 
  count()



