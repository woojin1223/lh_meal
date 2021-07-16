lunch_menus <- lunch_score_dict %>% names()
dinner_menus <- dinner_score_dict %>% names()
menus <- union(lunch_menus, dinner_menus) %>% sort()

test_lunch_menus <- test$Lunch %>% get_refined_menu_vec()
test_dinner_menus <- test$Dinner %>% get_refined_menu_vec()

test_menus <- union(test_lunch_menus, test_dinner_menus) %>% sort()

test_menus[!(test_menus %in% menus)]

library(dplyr)
library(magrittr)
library(readr)
library(stringr)

source("./functions_wj.R")

train <- read_csv("./data/train_wj.csv")
train %<>% 
    mutate(
        Lunch  = get_refined_menu(Lunch),
        Dinner = get_refined_menu(Dinner),
    )

write_csv(train, "./data/train_wj.csv")

test <- read_csv("./data/test_wj.csv")
test %<>% 
    mutate(
        Lunch  = get_refined_menu(Lunch),
        Dinner = get_refined_menu(Dinner),
    )

write_csv(test, "./data/test_wj.csv")


