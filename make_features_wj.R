library(dplyr)
library(magrittr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

source("./functions_wj.R")

# train -------------------------------------------------------------------

train <- read_csv("./data/train.csv")

train %<>% rename(
    Date      = 일자,
    Dayofweek = 요일,
    Total     = 본사정원수,
    Vacation  = 본사휴가자수,
    Business  = 본사출장자수,
    Overtime  = 본사시간외근무명령서승인건수,
    Home      = 현본사소속재택근무자수,
    Breakfast = 조식메뉴,
    Lunch     = 중식메뉴,
    Dinner    = 석식메뉴,
    N_lunch   = 중식계,
    N_dinner  = 석식계
)

train %<>% replace_na(list(Dinner = ""))

train %<>% mutate(
    Dayofweek     = factor(Dayofweek, levels = c("월", "화", "수", "목", "금"), labels = c(1, 2, 3, 4, 5)),
    Net_total     = Total - (Vacation+Business+Home),
    Rate_vacation = Vacation / Net_total,
    Rate_business = Business / Net_total,
    Rate_overtime = Overtime / Net_total,
    Rate_lunch    = N_lunch / Net_total,
    Rate_dinner   = N_dinner / Net_total,
    New_lunch     = get_new_menu(Lunch),
    New_dinner    = get_new_menu(Dinner)
)

train %<>% get_menu_encoding("Lunch", menu_encoding_dict)
train %<>% get_menu_encoding("Dinner", menu_encoding_dict)

lunch_score_dict <- get_score_dict(train$Lunch, train$Rate_lunch)
dinner_score_dict <- get_score_dict(train$Dinner, train$Rate_dinner)

train %<>% mutate(
    Lunch_score  = get_score(Lunch, lunch_score_dict),
    Dinner_score = get_score(Dinner, dinner_score_dict)
)

menu_encoding_dict <- get_menu_encoding_dict()

write_csv(train, file = "./data/train_wj.csv")

# test --------------------------------------------------------------------

test <- read_csv("./data/test.csv")
test %<>% rename(
    Date      = 일자,
    Dayofweek = 요일,
    Total     = 본사정원수,
    Vacation  = 본사휴가자수,
    Business  = 본사출장자수,
    Overtime  = 본사시간외근무명령서승인건수,
    Home      = 현본사소속재택근무자수,
    Breakfast = 조식메뉴,
    Lunch     = 중식메뉴,
    Dinner    = 석식메뉴
)

test %<>% mutate(
    Dayofweek     = factor(Dayofweek, levels = c("월", "화", "수", "목", "금"), labels = c(1, 2, 3, 4, 5)),
    Net_total     = Total - (Vacation+Home),
    Rate_vacation = Vacation / Net_total,
    Rate_business = Business / Net_total,
    Rate_overtime = Overtime / Net_total,
    New_lunch     = get_new_menu(Lunch),
    New_dinner    = get_new_menu(Dinner)
)

test %<>% get_menu_encoding("Lunch", menu_encoding_dict)
test %<>% get_menu_encoding("Dinner", menu_encoding_dict)

lunch_sim_matrix <- get_sim_matrix(test$Lunch, "Lunch", lunch_score_dict)
dinner_sim_matrix <- get_sim_matrix(test$Dinner, "Dinner", dinner_score_dict)

lunch_score_dict %<>% get_update_score_dict(lunch_sim_matrix)
dinner_score_dict %<>% get_update_score_dict(dinner_sim_matrix)

test %<>% mutate(
    Lunch_score  = get_score(Lunch, lunch_score_dict),
    Dinner_score = get_score(Dinner, dinner_score_dict)
)

write_csv(test, file = "./data/test_wj.csv")
