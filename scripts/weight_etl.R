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

files <- list("data/Renpho-Jeremy Butt.csv",
              "/Users/jeremybutt/Downloads/Renpho-Jeremy Butt.csv")

weight_data_list <- lapply(X = files,
                           FUN = read_csv,
                           col_types = cols(`Time of Measurement` = col_character(),
                                            `Weight(lb)` = col_double(),
                                            BMI = col_double(),
                                            `Body Fat(%)` = col_double(),
                                            `Fat-free Body Weight(lb)` = col_double(),
                                            `Subcutaneous Fat(%)` = col_double(),
                                            `Visceral Fat` = col_double(),
                                            `Body Water(%)` = col_double(),
                                            `Skeletal Muscle(%)` = col_double(),
                                            `Muscle Mass(lb)` = col_double(),
                                            `Bone Mass(lb)` = col_double(),
                                            `Protein(%)` = col_double(),
                                            `BMR(kcal)` = col_double(),
                                            `Metabolic Age` = col_double(),
                                            Remarks = col_character())) %>%
  map(~ .x %>%
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
                                      replacement = "_")))


#READING NEW DATA INTO R
weight_data_file_current <- read_csv(weight_data_file_current_file) %>%
  anti_join(weight_data_hist, by = "Time of Measurement") #REMOVING DUPLICATE DATA

weight_data <- weight_data_hist %>%
  bind_rows(weight_data_file_current)

weight_data <- weight_data %>%
  mutate(date = mdy_hms(time_of_measurement))


glimpse(weight_data)


plot(x = weight_data$date, y = weight_data$weight_lb)
