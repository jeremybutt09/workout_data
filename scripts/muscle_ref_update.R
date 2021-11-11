#LIBRARY LOAD
library(tidyverse)

#RELEVANT FILES
muscle_reference_file <- "reference/muscle_ref.csv"
wo_data_file <- "data/workout_output.csv"

#INITAL READING INTO OF RELEVANT FILES
muscle_ref <- read_csv(muscle_reference_file)
wo_data <- read_csv(wo_data_file)

#COMPARING EXERCISES IN MUSCLE REFERENCE TO EXERICSES IN WORKOUT DATA
muscle_ref_distinct_exercise_names <- muscle_ref %>%
  distinct(exercise_name)

wo_data_distinct_exercise_names <- wo_data %>%
  distinct(exercise_name)

new_exercise_names <- wo_data_distinct_exercise_names %>%
  anti_join(muscle_ref_distinct_exercise_names, by = "exercise_name")

new_exercise_df <- tibble(exercise_name = new_exercise_names,
                          generic_muscle_group = NA,
                          main_muscle = NA,
                          secondary_muscle = NA)

glimpse(new_exercise_names)

write_csv(x = new_exercise_names,
          file = muscle_reference_file,
          append = TRUE)
