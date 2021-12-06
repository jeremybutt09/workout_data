library(tidyverse)
library(lubridate)

wo_data <- read_csv(file = "data/workout_output.csv")
ref_data <- read_csv(file = "reference/muscle_ref.csv")

wo_data_agg <- wo_data %>%
  left_join(ref_data, by = "exercise_name") %>%
  mutate(workout_week = floor_date(workout_date, "week")) %>%
  group_by(workout_week,
           generic_muscle_group) %>%
  summarise(volume = sum(volume))

glimpse(wo_data_agg)

ggplot(data = wo_data_agg, aes(x = workout_week, y = volume, fill = generic_muscle_group)) +
  geom_col() +
  facet_wrap(~generic_muscle_group)

unique(ref_data$generic_muscle_group)
