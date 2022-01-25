#AUTHOR: JEREMY BUTT
#DATE CREATED: 2021-09-15

#UPDATE: SEE GIT COMMENTS

#KNOWN ISSUES/LIMITATION:
#-NO WAY OF CHECKING IF DATA BEING IMPORTED IS DUPLICATE.
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

#EXTRACT DATE FROM NOTE
workout_date <- workout_content %>%
  map(~str_extract(string = .x,
                   pattern = "(?<=DATE:[[:space:]]{0,5})[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}") %>%
        unlist() %>%
        str_trim(.))

#EXTRACT TITLES FROM NOTES
workout_titles <- workout_content %>%
  map(~str_extract(string = .x,
                   pattern = ".+") %>%
        str_trim(.))

#EXTRACT EXERCISE NUMBERS FROM NOTES
workout_number <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "EXERCISE [[:digit:]]{1}\\.?[[:digit:]]?(?=:)") %>%
        unlist(.) %>%
        str_remove_all(string = .,
                       pattern = "[[:alpha:]]") %>%
        str_trim())

#EXTRACT EXERCISE NAMES FROM NOTES
workout_exercises <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "(?<=EXERCISE [[:digit:]]{1}\\.?[[:digit:]]?:[[:space:]]{0,5}).+") %>%
        unlist(.) %>%
        str_trim(.))

#EXTRACT SET NUMBERS FROM NOTES
set_number <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "SET [[:digit:]]{1,2}") %>%
        unlist(.) %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               set_number = .) %>%
        mutate_if(is.character, str_trim))

#EXTRACT SET DATA (WEIGHT ANDS REPS) FROM NOTES
set_data <- workout_content %>%
  map(~str_extract_all(string = .x,
                       pattern = "(?<=SET [[:digit:]]{1,2}:[^[:alnum:]]{0,5})[[:digit:]]{1,3}\\.?[[:digit:]]{1,2}[[:space:]]{0,5}X[[:space:]]{0,5}[[:digit:]]{1,3}") %>%
        unlist(.) %>%
        str_trim(.) %>%
        data.frame() %>%
        rename(.data = .,
               set_data = .) %>%
        mutate_if(is.character, str_trim))

#CREATING A DATAFRAME THAT WILL CONTAIN WORKOUT EXERCISE AKA NAME OF EXERCISES IN WORKOUT, WITH THE NUMBER OF SETS
#COMPLETE AND THE WEIGHTS CORRESPONDING TO THE SETS
set_data_df <- vector(mode = "list", length = length(workout_content))
set_1_index <- vector(mode = "list", length = length(workout_content))

for (i in 1:length(workout_content)) {

    set_data_df[[i]] <- bind_cols(set_number[[i]], set_data[[i]]) %>%
      transmute(workout_date = workout_date[[i]],
                workout_titles = workout_titles[[i]],
                exercise_number = NA,
                workout_exercises = NA,
                set_number,
                set_data) %>%
      separate(col = set_data,
               into = c("weight", "reps"),
               sep = "X",
               convert = TRUE)
    
    set_1_index[[i]] <- which(set_data_df[[i]]$set_number == "SET 1")
  

  for(j in 1:length(set_1_index[[i]])) {
    
    set_data_df[[i]][set_1_index[[i]][j], 3] <- workout_number[[i]][j]
    set_data_df[[i]][set_1_index[[i]][j], 4] <- workout_exercises[[i]][j]
    
    }
  
  set_data_df[[i]] <- set_data_df[[i]] %>%
    fill(exercise_number,
         workout_exercises) %>%
    mutate(volume = weight*reps,
           set_number = str_extract(string = set_number,
                                    pattern = "[[:number:]]+"))

}


output_data <- bind_rows(set_data_df)

#APPEND DATA TO CSV
write_csv(x = output_data,
          path = output_file,
          append = TRUE)

#REMOVING WORKOUT FILES. FILES HAVE BEEN PROCESSED THERE IS NO NEED TO KEEP OVER FILES.
file.remove(workout_files)


