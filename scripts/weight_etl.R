#AUTHOR: JEREMY BUTT
#DATE CREATED: 2021-09-15

#UPDATE: SEE GIT COMMENTS

#KNOWN ISSUES/LIMITATION:
#-NO WAY OF CHECKING IF DATA BEING IMPORTED IS DUPLICATE.
#-HAVE CODE CHECK IF DATA HAS ALREADY BEEN IMPORTED.

#LIBRARY LOAD
library(tidyverse)
library(lubridate)

#FILES FOR WEIGHT DATA. 

#WEIGHT DATA HISTORIC
#weight_data_file_hist <- "data/Renpho-Jeremy Butt.csv"

#NEWLY IMPORTED DATA.
#weight_data_file_current_file <- "/Users/jeremybutt/Downloads/Renpho-Jeremy Butt.csv"

files <- list("data/weight_data.csv",
              "/Users/jeremybutt/Downloads/Renpho-Jeremy Butt.csv")

weight_data_list <- lapply(X = files,
                           FUN = read_csv,
                           col_types = list(col_character(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_double(),
                                            col_character()))

weight_data_list[[1]] <- weight_data_list[[1]] %>%
  mutate(date_time = as_date(date_time))

weight_data_list[[2]] <- weight_data_list[[2]] %>%
        rename_with(str_to_lower) %>%
        rename_with(~ str_replace_all(.x,
                                      pattern = "%",
                                      replacement = "percent")) %>%
        rename_with(~ str_replace_all(.x,
                                      pattern = "[[:punct:]]",
                                      replacement = " ")) %>%
        rename_with(str_trim) %>%
        rename_with(~ str_replace_all(.x,
                                      pattern = "[[:space:]]",
                                      replacement = "_")) %>%
        rename(date_time = time_of_measurement) %>%
        mutate(date_time = mdy_hms(date_time),
               date = as_date(date_time))

weight_data_list[[3]] <- weight_data_list[[2]] %>% #NEW WEIGHT DATA IMPORTED
  anti_join(weight_data_list[[1]], by = "date") #REMOVING DUPLICATE DATA

weight_data <- bind_rows(weight_data_list[[1]], weight_data_list[[3]]) %>%
  arrange(date_time)

write_csv(x = weight_data,
          file = files[[1]])

file.remove(file = files[[2]])
