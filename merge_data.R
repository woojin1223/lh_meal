library(readr)
library(dplyr)

train_wj <- read_csv("./data/train_wj.csv")
test_wj <- read_csv("./data/test_wj.csv")

# train_jh <- read_csv("./data/train_jh.csv")
# test_jh <- read_csv("./data/test_jh.csv")

# train_wj <- train
# test_wj <- test

train_wj <- bind_cols(train_wj, train_jh[c("Before_holiday", "After_holiday", "Sandwich_day")])
test_wj <- bind_cols(test_wj, test_jh[c("Before_holiday", "After_holiday", "Sandwich_day")])

write_csv(train_wj, path = "./data/train_wj.csv")
write_csv(test_wj, path = "./data/test_wj.csv")