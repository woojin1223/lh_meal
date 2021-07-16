train_lunch_df <- tibble(Lunch = names(lunch_score_dict), score = as.numeric(lunch_score_dict))
train_lunch_df <- get_menu_encoding(train_lunch_df, "Lunch", menu_encoding_dict)
train_lunch_matrix <- select(train_lunch_df, starts_with("Lunch_")) %>% as.matrix()

test_lunch_menus <- test$Lunch %>% get_refined_menu_vec() %>% unique()
new_lunch_menus <- test_lunch_menus[test_lunch_menus %!in% names(lunch_score_dict)]
new_lunch_df <- tibble(Lunch = new_lunch_menus, score = NA)
new_lunch_df <- get_menu_encoding(new_lunch_df, "Lunch", menu_encoding_dict)
new_lunch_matrix <- select(new_lunch_df, starts_with("Lunch_")) %>% as.matrix()

sim_matrix <- as_tibble(matrix(0, nrow = nrow(train_lunch_df), ncol = nrow(new_lunch_df)))
colnames(sim_matrix) <- new_lunch_df$Lunch

for (i in 1:nrow(new_lunch_df)) {
    sim_matrix[, i] <- apply(train_lunch_matrix, 1, function(x) jaccard(x, new_lunch_matrix[i, ]))
}

sim_matrix %<>% as_tibble() %>% mutate(menu = train_lunch_df$Lunch)



lunch_sim_matrix <- get_sim_matrix(test$Lunch, "Lunch", lunch_score_dict)

lunch_score_dict <- get_update_score_dict(sim_matrix, lunch_score_dict)

