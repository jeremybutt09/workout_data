#AUTHOR: JEREMY BUTT
#DATE CREATED: 2021-10-31

#UPDATE: SEE GIT COMMENTS

#KNOWN ISSUES/LIMITATION:


#LIBRARY LOAD
library(tidyverse)
library(lubridate)


wo_data <- read_csv(file = "data/workout_output.csv")
ref_data <- read_csv(file = "reference/muscle_ref.csv")

set_pr_long <- wo_data %>%
  filter(reps != 0,
         exercise_name == "FLAT BENCH PRESS (BARBELL)") %>%
  group_by(exercise_name,
           set,
           reps) %>%
  summarize(pr = max(weight)) %>%
  inner_join(wo_data, by = c("exercise_name", "reps", "set")) %>%
  group_by(exercise_name,
           set,
           reps,
           pr) %>%
  summarize(date = max(workout_date))

pr_wide <- set_pr_long %>%
  transmute(exercise_name,
            set,
            reps,
            pr_details = paste("PR:", pr, "LBS", "DATE:", date)) %>%
  pivot_wider(names_from = exercise_name,
              values_from = pr_details) %>%
  arrange(reps)

max_rep_weight_set_graph <- wo_data %>%
  filter(reps != 0) %>%
  group_by(exercise_name,
           set,
           reps,
           workout_date) %>%
  summarize(pr = max(weight)) %>%
  filter(exercise_name %in% c("LAT PULLDOWN (MACHINE)"))

ggplot(data = max_rep_weight_set_graph, aes(x = workout_date, y = pr, color = as.character(reps))) +
  geom_line() +
  geom_point() +
  facet_wrap(~as.character(set))
