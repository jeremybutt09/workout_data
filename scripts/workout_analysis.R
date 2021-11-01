#AUTHOR: JEREMY BUTT
#DATE CREATED: 2021-10-31

#UPDATE: SEE GIT COMMENTS

#KNOWN ISSUES/LIMITATION:


#LIBRARY LOAD
library(tidyverse)
library(lubridate)

#OUTPUT FILE WHERE FILE IS SAVED
wo_data_file <- "/Users/jeremybutt/workout_data/data/workout_output.csv"

wo_data <- read_csv(file = wo_data_file)

weekly_agg <- wo_data %>%
  mutate(date = floor_date(x = workout_date,
                                        unit = 'week')) %>%
  group_by(date) %>%
  summarise(num_of_workouts = n_distinct(workout_date),
            num_of_exercises = n_distinct(exercise_name),
            total_weight = sum(weight),
            total_reps = sum(reps),
            total_volume = sum(volume)) %>%
  pivot_longer(cols = -date,
               names_to = "agg_description",
               values_to = "values")

#CREATING PLOTS
ggplot(data = weekly_agg, aes(x = date, y = values, color = agg_description, fill = agg_description)) +
  #geom_point() +
  #geom_line() +
  geom_col() +
  facet_wrap(~agg_description, scales = "free")
  
