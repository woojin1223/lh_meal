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
        Lunch = get_refined_menu(Lunch),
        Dinner = get_refined_menu(Dinner),
    )

write_csv(train, "./data/train_wj.csv")

test <- read_csv("./data/test_wj.csv")
test %<>% 
    mutate(
        Lunch = get_refined_menu(Lunch),
        Dinner = get_refined_menu(Dinner),
    )

write_csv(test, "./data/test_wj.csv")

lunch_score_df <- tibble(Lunch = names(lunch_score_dict), score = as.numeric(lunch_score_dict))
lunch_score_df <- get_menu_encoding(lunch_score_df, "Lunch", menu_encoding_dict)
lunch_matrix <- select(lunch_score_df, starts_with("Lunch_")) %>% as.matrix()

test_lunch_menus <- test$Lunch %>% get_refined_menu_vec()
new_lunch <- test_lunch_menus[test_lunch_menus %!in% names(lunch_score_dict)] %>% unique()
new_lunch_score_df <- tibble(Lunch = new_lunch, score = NA)
new_lunch_score_df <- get_menu_encoding(new_lunch_score_df, "Lunch", menu_encoding_dict)
new_lunch_matrix <- select(new_lunch_score_df, starts_with("Lunch_")) %>% as.matrix()

new_lunch_score_df$n_menu <- new_lunch_score_df %>% select(starts_with("Lunch_")) %>% rowSums()

sim_matrix <- as_tibble(matrix(0, nrow = nrow(lunch_score_df), ncol = nrow(new_lunch_score_df)))
colnames(sim_matrix) <- new_lunch_score_df$Lunch

for (i in 1:nrow(new_lunch_matrix)) {
    sim_matrix[, i] <- apply(lunch_matrix, 1, function(x) jaccard(x, new_lunch_matrix[i, ]))
}

sim_matrix %<>% as_tibble() %>% mutate(rowname = lunch_score_df$Lunch)

sim_matrix %>% select(c("수제삼색무쌈", "rowname")) %>% arrange(desc(수제삼색무쌈)) %>% slice(1:5)
