#AUTHOR: JEREMY BUTT
#DATE CREATED: 2021-09-15

#UPDATE: SEE GIT COMMENTS

#KNOWN ISSUES/LIMITATION:
#-CURRENTLY IN NOTES ALL EXERCISES NEED TO HAVE EXACTLY 5 SETS. NO MORE NO LESS.
#-SETS THAT WERE NOT COMPLETED NEED TO HAVE 0 X 0
#-NO WAY OF CHECKING IF DATA BEING IMPORTED IS DUPLICATE.
#-JOIN DATA TO REFERENCE TABLE
#-HAVE CODE CHECK IF DATA HAS ALREADY BEEN IMPORTED.

#LIBRARY LOAD
library(tidyverse)
library(lubridate)

#DEFINE TODAY SUFFIX YYYYMMDD
today <- today() %>%
  format("%Y%m%d")

#FOLDER THAT CONTAIN WORKOUT DATA IN TEXT FILE FORMAT
workout_data_folder <- "/Users/jeremybutt/Desktop/Workout_data"

#OUTPUT FILE WHERE FILE IS SAVED
output_file <- "/Users/jeremybutt/workout_data/data/workout_output.csv"

#LIST OF TEXT FILES IN THE WORKOUT DATA FOLDER.
#SHOULD FIGURE OUT A WAY TO LIMIT WHAT IS RETURNED. COULD ACCOMPLISH THIS BY LOOKING AT DATABASE/SPREADSHEET.
workout_files <- list.files(workout_data_folder,
                            full.names = TRUE,
                            pattern = "_current")

#READING THE FILES INTO R. AGAIN SHOULD LIKE WHAT IS BEING BROUGHT IN
workout_content <- lapply(X = workout_files,
                          FUN = read_file) %>%
  map(str_to_upper)

#IDEA IS TO BRING ALL ATTRIBUTES INTO R IN THEIR OWN SEPERATE VARIABLE THEN FROM THERE CAN MAKE A DATA FRAME FROM ALL THE 
#INDIVIDUAL VARIABLE

#EXTRACT DATE FROM NOTE
workout_date <- workout_content %>%
  map(~str_extract(string = .x,
                   pattern = "(?<=DATE:[[:space:]]{0,5})[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}") %>%
        unlist() %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               workout_date = .) %>%
        mutate_if(is.character, str_trim))

#EXTRACT TITLES FROM NOTES
workout_titles <- workout_content %>%
  map(~str_extract(string = .x,
                   pattern = ".+") %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               work_titles = .) %>%
        mutate_if(is.character, str_trim))

#EXTRACT EXERCISE NUMBERS FROM NOTES
workout_number <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "EXERCISE [[:digit:]]{1}\\.?[[:digit:]]?(?=:)") %>%
        unlist(.) %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               exercise_number = .) %>%
        mutate(exercise_number = str_remove_all(string = exercise_number,
                                                pattern = "[[:alpha:]]")) %>%
        mutate_if(is.character, str_trim))
  

#EXTRACT EXERCISE NAMES FROM NOTES
workout_exercises <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "(?<=EXERCISE [[:digit:]]{1}\\.?[[:digit:]]?:[[:space:]]{0,5}).+") %>%
        unlist(.) %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               exercise_name = .) %>%
        mutate_if(is.character, str_trim))

#EXTRACT SET DATA FROM WORKOUT CONTENT. THIS CAN BE IMPROVED. NEED TO THINK HOW TO DO IT CORRECTLY BUT LIKELY THROUGH A LOOP
#FOR NOT ASSUMING NO MORE THAN 5 SETS FOR A SINGLE EXERCISE.
workout_set1 <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "(?<=SET 1:[^[:alnum:]]{0,5})[[:digit:]]{1,3}[[:space:]]{0,5}X[[:space:]]{0,5}[[:digit:]]{1,3}") %>%
        unlist(.) %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               set1_data = .) %>%
        separate(col = set1_data,
                 into = c("set_1_weight", "set_1_reps"),
                 sep = "X") %>%
        mutate_if(is.character, str_trim))

workout_set2 <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "(?<=SET 2:[^[:alnum:]]{0,5})[[:digit:]]{1,3}[[:space:]]{0,5}X[[:space:]]{0,5}[[:digit:]]{1,3}") %>%
        unlist(.) %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               set_data = .) %>%
        separate(col = set_data,
                 into = c("set_2_weight", "set_2_reps"),
                 sep = "X") %>%
        mutate_if(is.character, str_trim))

workout_set3 <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "(?<=SET 3:[^[:alnum:]]{0,5})[[:digit:]]{1,3}[[:space:]]{0,5}X[[:space:]]{0,5}[[:digit:]]{1,3}") %>%
        unlist(.) %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               set_data = .) %>%
        separate(col = set_data,
                 into = c("set_3_weight", "set_3_reps"),
                 sep = "X") %>%
        mutate_if(is.character, str_trim))

workout_set4 <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "(?<=SET 4:[^[:alnum:]]{0,5})[[:digit:]]{1,3}[[:space:]]{0,5}X[[:space:]]{0,5}[[:digit:]]{1,3}") %>%
        unlist(.) %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               set_data = .) %>%
        separate(col = set_data,
                 into = c("set_4_weight", "set_4_reps"),
                 sep = "X") %>%
        mutate_if(is.character, str_trim))

workout_set5 <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "(?<=SET 5:[^[:alnum:]]{0,5})[[:digit:]]{1,3}[[:space:]]{0,5}X[[:space:]]{0,5}[[:digit:]]{1,3}") %>%
        unlist(.) %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               set_data = .) %>%
        separate(col = set_data,
                 into = c("set_5_weight", "set_5_reps"),
                 sep = "X") %>%
        mutate_if(is.character, str_trim))

df_list <- vector(mode = "list",
                  length = length(workout_titles))

for (i in 1:length(workout_titles)) {
  df_list[[i]] <- data.frame(workout_date[[i]], 
                             workout_titles[[i]], 
                             workout_number[[i]],
                             workout_exercises[[i]], 
                             workout_set1[[i]],
                             workout_set2[[i]],
                             workout_set3[[i]],
                             workout_set4[[i]],
                             workout_set5[[i]])
}

output_data <- df_list %>%
  map(~pivot_longer(data = .x,
                    cols = starts_with("set"),
                    names_to = "set_info",
                    values_to = "value") %>%
        separate(col = set_info,
                 sep = "_(?=[[:alpha:]])",
                 into = c("set", "description")) %>%
        mutate(set = str_extract(string = set,
                                 pattern = "[[:digit:]]+")) %>%
        pivot_wider(names_from = description,
                    values_from = value) %>%
        mutate(volume = as.numeric(weight) * as.numeric(reps)) %>%
        mutate_all(as.character)) %>%
  bind_rows()

#APPEND DATA TO CSV
write_csv(x = output_data,
          path = output_file,
          append = TRUE)

#REMOVING WORKOUT FILES. FILES HAVE BEEN PROCESSED THERE IS NO NEED TO KEEP OVER FILES.
file.remove(workout_files)


