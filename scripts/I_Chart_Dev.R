library(tidyverse)

weight_file <- "data/weight_data.csv"

weight_data <- read_csv(weight_file)

weight_vector <- weight_data %>%
  filter(date >= "2021-11-01") %>%
  group_by(date) %>%
  summarize(weight = max(weight_lb)) %>%
  pull(weight)

mr <- abs(diff(weight_vector))
mr

sum_mr <- sum(mr)
sum_mr  

k <- length(mr) + 1

mr_bar <- sum_mr/(k-1)
mr_bar

ul_mr <- 3.27*mr_bar
ul_mr

mr <- mr[mr <= ul_mr]

sum_mr <- sum(mr)
sum_mr  

k <- length(mr) + 1

mr_bar <- sum_mr/(k-1)
mr_bar

ul_mr <- 3.27*mr_bar
ul_mr

i_bar <- sum(weight_vector)/k
i_bar

ul = i_bar + (2.66*mr_bar)
ul

ll = i_bar - (2.66*mr_bar)
ll
