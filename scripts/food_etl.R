#LOAD LIBRARIES
library(tidyverse)
library(lubridate)

#FILE VARIABLE

#WHEN EXPORT FROM NETDIARY THIS IS THE FILE
net_diary_file <- "C:/Users/Jeremy/Downloads/MyNetDiary_Year_2022.xls"

#NAME OF THE OUTPUT FILE WHERE DATA WILL BE APPENDED
output_file <- "data/food_data.csv"

#READING HISTORIC FOOD DATA INTO R. THIS WILL BE USED TO IDENTIFY NEW DATA AND
#AVOID DUPLICATION OF DATA
food_data <- read_csv(output_file)

#READING NEW FOOD ENTRIES INTO R. RUNNING SOME FUNCTIONS TO CLEAN UP THE COLUMN
#NAMES
food_data_new <- readxl::read_xls(net_diary_file) %>%
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

#COMPLETING AN ANTI-JOIN TO RETURN ONLY ENTRIES THAT DO NOT CURRENTLY EXIST IN
#THE HISTORIC DATA
food_data_import <- food_data_new %>%
  anti_join(food_data, by = "date_time")

#APPENDING DATA TO THE OUTPUT FILE
write_csv(x = food_data_import,
          file = output_file,
          append = TRUE)

#REMOVE FILE
file.remove(net_diary_file)