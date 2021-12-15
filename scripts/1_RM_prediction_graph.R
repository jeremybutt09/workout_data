library(tidyverse)
library(lubridate)

wo_data_url <- "C:/Users/Jeremy/Documents/workout_data/data/workout_output.csv"
ref_data_url <- "C:/Users/Jeremy/Documents/workout_data/reference/muscle_ref.csv"
rm_pred_url <- "C:/Users/Jeremy/Documents/workout_data/reference/1_RM_PREDICTION.csv"

wo_data <- read_csv(file = wo_data_url) %>%
  mutate(reps_factor = factor(x = as.character(reps),
                              levels = c(1:20)))
ref_data <- read_csv(file = ref_data_url)
rm_pred <- read_csv(file = rm_pred_url)

exercises <- c("BACK SQUAT (BARBELL)",
               "DEADLIFT (BARBELL)",
               "FLAT BENCH PRESS (BARBELL)",
               "STANDING SHOULDER PRESS (BARBELL)",
               "LAT PULLDOWN (MACHINE)")


#EXTRACTS THE HEAVEST WEIGHT LIFTED FOR A GIVEN WEEK AND THE NUMBER OF REPS PERFORMED. THEN CALCUALTES PREDICTED 1 RM
rm_by_week <- wo_data %>%
  filter(exercise_name %in% exercises) %>%
  mutate(date_floor_week = floor_date(workout_date, 'week')) %>%
  group_by(date_floor_week,
           exercise_name) %>%
  mutate(max_weight = max(weight)) %>%
  filter(weight == max_weight) %>%
  group_by(date_floor_week,
           exercise_name,
           max_weight) %>%
  summarize(max_reps = max(reps)) %>%
  inner_join(rm_pred, by = c("max_reps" = "REPS")) %>%
  mutate(predicted_1RM = round(x = max_weight/PERCENT_1_RM,
                               digits = 0))

#PLOTTING PREDICTED 1 RM SCORE
ggplot(data = rm_by_week, aes(x = date_floor_week , y = predicted_1RM, color = exercise_name)) +
  geom_line(size = 1.25) +
  geom_point(size = 2) +
  facet_wrap(~exercise_name)

#CALCULATING PERCENT CHANGE IN PREDICTED 1 RM OVER TIME
rm_by_week_percent_change <- wo_data %>%
  filter(exercise_name %in% exercises) %>%
  mutate(date_floor_week = floor_date(workout_date, 'week')) %>%
  group_by(date_floor_week,
           exercise_name) %>%
  mutate(max_weight = max(weight)) %>%
  filter(weight == max_weight) %>%
  group_by(date_floor_week,
           exercise_name,
           max_weight) %>%
  summarize(max_reps = max(reps)) %>%
  inner_join(rm_pred, by = c("max_reps" = "REPS")) %>%
  mutate(predicted_1RM = round(x = max_weight/PERCENT_1_RM,
                               digits = 0)) %>%
  arrange(date_floor_week,
             exercise_name) %>%
  group_by(exercise_name) %>%
  mutate(pct_change = ((predicted_1RM-lag(predicted_1RM))/predicted_1RM)*100)

ggplot(data = rm_by_week_percent_change, aes(x = date_floor_week, pct_change, color = exercise_name)) +
  geom_line(size = 1.25) +
  geom_point(size = 2) +
  facet_wrap(~exercise_name)

glimpse(rm_by_week_percent_change)
