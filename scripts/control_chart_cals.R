library(tidyverse)
library(lubridate)

weight_data <- read_csv("data/weight_data.csv") %>%
  select(date,
         weight_lb) %>%
  filter(date >= ymd("2021-10-01")  & date <= ymd("2022-01-24")) %>%
  #mutate(stage = if_else(date >= ymd("2021-10-01") & date <= ymd("2022-01-24"),
  #                       1,
  #                       0)) %>%
  group_by(date) %>%
           #stage) %>%
  summarise(weight = max(weight_lb)) %>%
  ungroup() %>%
  mutate(mr = abs(lag(weight) - weight))

glimpse(weight_data)

mr_bar <- mean(weight_data$mr, na.rm = TRUE)

ul_mr <- 3.27*mr_bar

mr_new <- weight_data$mr[weight_data$mr <= ul_mr]

mr_bar_new <- mean(mr_new, na.rm = TRUE)


read_csv("data/weight_data.csv") %>%
  select(date,
         weight_lb) %>%
  filter(date >= ymd("2021-10-01")) %>%
  group_by(date) %>%
  summarise(weight = max(weight_lb)) %>%
  ungroup() %>%
  write_csv(file = "data/weight_data_tableau.csv")
  