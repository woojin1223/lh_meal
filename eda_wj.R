library(dplyr)
library(magrittr)
library(purrr)
library(readr)
library(stringr)

source("./functions.R")

train <- read_csv("./data/train.csv")
train %<>% rename(
    date      = 일자,
    dow       = 요일,
    total     = 본사정원수,
    vacation  = 본사휴가자수,
    business  = 본사출장자수,
    overtime  = 본사시간외근무명령서승인건수,
    home      = 현본사소속재택근무자수,
    breakfast = 조식메뉴,
    lunch     = 중식메뉴,
    dinner    = 석식메뉴,
    lunch_n   = 중식계,
    dinner_n  = 석식계
)

train %<>% mutate(
    dow             = factor(dow, levels = c("월", "화", "수", "목", "금"), labels = c(1, 2, 3, 4, 5)),
    net_total       = total - (vacation+business+home),
    lunch_rate      = lunch_n / net_total,
    dinnter_rate    = dinner_n / net_total,
    new_lunch_menu  = get_new_menu(lunch),
    new_dinner_menu = get_new_menu(dinner)
)

lunch_score_dict <- get_score_dict(train$lunch, train$lunch_rate)
dinner_score_dict <- get_score_dict(train$dinner, train$dinner_rate)

train %<>% mutate(
    lunch_score  = get_score(lunch, lunch_score_dict),
    dinner_score = get_score(dinner, dinner_score_dict)
)

test <- read_csv("./data/test.csv")
test %<>% rename(
    date      = 일자,
    dow       = 요일,
    total     = 본사정원수,
    vacation  = 본사휴가자수,
    business  = 본사출장자수,
    overtime  = 본사시간외근무명령서승인건수,
    home      = 현본사소속재택근무자수,
    breakfast = 조식메뉴,
    lunch     = 중식메뉴,
    dinner    = 석식메뉴
)