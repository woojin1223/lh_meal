library(readr)

train_wj <- read_csv("./data/train_wj.csv")

# 저녁 메뉴는 존재하지만 석식계가 없는 경우

train_wj[train_wj$N_dinner == 0 & train_wj$Dinner != "", "Dinner"] <- ""

# 점심 메뉴는 존재하지만 중식계가 없는 경우 (존재 x)

train_wj[train_wj$N_lunch == 0 & train_wj$Lunch != "", "Lunch"] <- ""