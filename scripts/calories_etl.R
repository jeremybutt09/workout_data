#AUTHOR: JEREMY BUTT
#DATE CREATED: 2022-03-16

#UPDATE: SEE GIT COMMENTS

#KNOWN ISSUES/LIMITATION:
#LIBRARY LOAD
library(tidyverse)
library(lubridate)

#FILES FOR WEIGHT DATA. 
files <- list("data/calorie_data.csv",
              "/Users/jeremybutt/Downloads/Health Data.csv")

calories_data_list <- lapply(X = files,
                             FUN = read_csv)

calories_data_list[[2]] <- calories_data_list[[2]] %>%
  rename_with(str_to_lower) %>%
  rename_with(~ str_replace_all(.x,
                                pattern = "[[:punct:]]",
                                replacement = "")) %>%
  rename_with(str_trim) %>%
  rename_with(~ str_replace_all(.x,
                                pattern = "[[:space:]]",
                                replacement = "_")) %>%
  rename(date = start) %>%
  mutate(date = dmy_hm(date)) %>%
  transmute(date,
            active_calories_kcal,
            cycling_distance_km = cycling_distance_mi/1.6,
            movement_distance = distance_mi/1.6)
  
glimpse(calories_data_list[[1]])
glimpse(calories_data_list[[2]])

calories_data <- calories_data_list[[2]] %>% #NEW WEIGHT DATA IMPORTED
  anti_join(calories_data_list[[1]], by = "date") %>% #REMOVING DUPLICATE DATA
  arrange(date)

write_csv(x = calories_data[[1]],
          file = "data/calorie_data.csv",
          append = TRUE)

file.remove(file = files[[2]])
