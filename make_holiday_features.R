library(dplyr)
library(lubridate)
library(magrittr)
library(tidyr)


train_wj <- read_csv("./data/train_wj.csv")
test_wj <- read_csv("./data/test_wj.csv")
holiday <- read_csv("./data/holidays.csv")

# 주말 리스트
## 1년 일자를 생성하고 요일 표시하기
# 시작일 지정
start_date <- as_date("2016-01-01")
# 종료일 지정
end_date <- as_date("2021-06-30")

# 일단위로 일련의 날짜 생성하기
df <- tibble(Date = seq(start_date, end_date, by = 1))
df %<>% mutate(Holiday_name = weekdays(as_date(Date)))

myweek <- df %>% filter(Holiday_name %in% c("토요일", "일요일"))

# Holiday feature (normal day:1, before holiday:2, after holiday:3, sandwich day:4)
`%!in%` <- Negate(`%in%`)

sandwich_set <- holiday %>% 
    bind_rows(myweek) %>% 
    arrange(Date) %>% 
    mutate(Before_holiday = ifelse(Holiday_name %!in% "일요일", Date - days(1), NA) %>% as_date()) %>% 
    mutate(After_holiday = ifelse(Holiday_name %!in% "토요일", Date + days(1), NA) %>% as_date()) %>% 
    mutate(interval = (ymd(Date) - ymd(lag(Date))) %>% as.numeric()) %>% 
    # filter(interval == 1 & lag(interval) == 1 | interval == 1 & lead(interval) == 1 | interval == 2) %>% 
    mutate(Sandwich_day = ifelse(interval == 2, Date-days(1), NA) %>% as_date())

before_holiday <- sandwich_set %>% 
    select(Before_holiday, After_holiday, Sandwich_day) %>% 
    gather(key = Holiday_feature, value = value, 1:3, na.rm = T) %>% 
    filter(Holiday_feature == "Before_holiday") %>% 
    select(Date = value, Before_holiday = Holiday_feature) %>% 
    mutate(Before_holiday = 1) %>% 
    distinct()

after_holiday <- sandwich_set %>% 
    select(Before_holiday, After_holiday, Sandwich_day) %>% 
    gather(key = Holiday_feature, value = value, 1:3, na.rm = T) %>% 
    filter(Holiday_feature == "After_holiday") %>% 
    select(Date = value, After_holiday = Holiday_feature) %>% 
    mutate(After_holiday = 1) %>% 
    distinct()

sandwich_day <- sandwich_set %>% 
    select(Before_holiday, After_holiday, Sandwich_day) %>% 
    gather(key = Holiday_feature, value = value, 1:3, na.rm = T) %>% 
    filter(Holiday_feature == "Sandwich_day") %>% 
    select(Date = value, Sandwich_day = Holiday_feature) %>% 
    mutate(Sandwich_day = 1) %>% 
    distinct()

train <- read_csv("./data/train.csv")
test <- read_csv("./data/test.csv")

train_jh <- train %>% 
    left_join(before_holiday, by = "Date") %>% 
    mutate(Before_holiday = ifelse(is.na(Before_holiday), 0, Before_holiday)) %>%
    left_join(after_holiday, by = "Date") %>%
    mutate(After_holiday = ifelse(is.na(After_holiday), 0, After_holiday)) %>%
    left_join(sandwich_day, by = "Date") %>%
    mutate(Sandwich_day = ifelse(is.na(Sandwich_day), 0, Sandwich_day))

test_jh <- test %>% 
    left_join(before_holiday, by = "Date") %>% 
    mutate(Before_holiday = ifelse(is.na(Before_holiday), 0, Before_holiday)) %>%
    left_join(after_holiday, by = "Date") %>%
    mutate(After_holiday = ifelse(is.na(After_holiday), 0, After_holiday)) %>%
    left_join(sandwich_day, by = "Date") %>%
    mutate(Sandwich_day = ifelse(is.na(Sandwich_day), 0, Sandwich_day))

