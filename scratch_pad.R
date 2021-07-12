lunch_score_dict
dinner_score_dict

lunch_menus <- lunch_score_dict %>% names()
dinner_menus <- dinner_score_dict %>% names()

menus <- union(lunch_menus, dinner_menus) %>% sort()
