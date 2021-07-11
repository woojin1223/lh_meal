train_jh <- read_csv("./data/train_jh.csv")
test_jh <- read_csv("./data/test_jh.csv")

train_wj <- bind_cols(train, train_jh[c("Before_holiday", "After_holiday", "Sandwich_day")])
test_wj <- bind_cols(test, test_jh[c("Before_holiday", "After_holiday", "Sandwich_day")])


write_csv(train_wj, path = "./data/train_wj.csv")
write_csv(test_wj, path = "./data/test_wj.csv")
