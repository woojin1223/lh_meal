library(dict)
library(dplyr)
library(magrittr)
library(readr)
library(stringr)

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
    net_total    = total - (vacation+business),
    lunch_rate   = lunch_n / net_total,
    dinnter_rate = dinner_n / net_total
    )

get_refined_menu <- function(x) {
    x %<>% 
        str_remove_all(pattern = "\\(.{1,10}:.{1,10}\\)") %>%  # (쌀: 국내산) 등 제거 
        str_replace_all(pattern = "/", replacement = " ") %>%  # 쌀밥/잡곡밥 -> 쌀밥 잡곡밥
        str_replace_all(pattern = "  ", replacement = " ") %>% 
        str_replace_all(pattern = ",", replacement = " ") %>% 
        str_trim() %>% 
        str_split(pattern = " ") %>% 
        unlist()
    
    return(x)
    }

breakfast_menu <- get_refined_menu(train$breakfast)
lunch_menu     <- get_refined_menu(train$lunch)
dinner_menu    <- get_refined_menu(train$dinner)


temp <- train$lunch
temp %<>% str_remove_all(pattern = "\\(.{1,10}:.{1,10}\\)") # (쌀: 국내산) 등 제거
temp %<>% str_replace_all(pattern = "/", replacement = " ") # 쌀밥/잡곡밥 -> 쌀밥 잡곡밥
temp %<>% str_replace_all(pattern = "  ", replacement = " ")
temp %<>% str_replace_all(pattern = ",", replacement = " ")
temp %<>% str_trim()
temp %>% str_split(pattern = " ") %>% sapply(FUN = length) # 음식의 개수에 큰 차이가 없다.


lunch_menu %>% table() %>% as_tibble() %>% arrange(desc(n))

test <- read_csv("./data/test.csv")
