#LOAD LIBRARIES
library(tidyverse)
library(lubridate)

net_diary_file <- "C:/Users/Jeremy/Downloads/MyNetDiary_Year_2022.xls"

food_data <- readxl::read_xls(net_diary_file) %>%
  rename_with(str_to_lower) %>%
  rename_with(~ str_replace_all(.x,
                                pattern = "%",
                                replacement = "percent")) %>%
  rename_with(~ str_remove_all(.x,
                                pattern = "[[:punct:]]")) %>%
  rename_with(str_trim) %>%
  rename_with(~ str_replace_all(.x,
                                pattern = "[[:space:]]",
                                replacement = "_")) %>%
  rename_with(~ str_replace_all(.x,
                                pattern = "__",
                                replacement = "_")) %>%
  rename_with(str_trim)

glimpse(food_data)
