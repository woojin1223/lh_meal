lunch_score_dict
dinner_score_dict

lunch_menus <- lunch_score_dict %>% names()
dinner_menus <- dinner_score_dict %>% names()

menus <- union(lunch_menus, dinner_menus) %>% sort()

test_lunch_menus <- test$Lunch %>% get_refined_menu_vec()
test_dinner_menus <- test$Dinner %>% get_refined_menu_vec()

test_menus <- union(test_lunch_menus, test_dinner_menus) %>% sort()

test_menus[!(test_menus %in% menus)]


train %<>% mutate(Dayofweek = as.numeric(Dayofweek))
test %<>% mutate(Dayofweek = as.numeric(Dayofweek))
