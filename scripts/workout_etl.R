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

#FOR DEV WILL REMOVE PRIOR TO MERGING
workout_files <- "C:/Users/Jeremy/Documents/workout_data/data/set_extract_dev.txt"

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
      transmute(workout_date = NA,
                workout_titles = NA,
                exercise_number = NA,
                workout_exercises = NA,
                set_number,
                set_data) %>%
      separate(col = set_data,
               into = c("weight", "reps"),
               sep = "X")
    
    set_1_index[[i]] <- which(set_data_df[[i]]$set_number == "SET 1")
  
    #print(set_data_df)
    #print(set_1_index)
    #print(length(set_1_index[[i]]))
  for(j in 1:length(set_1_index[[i]])) {
    print(paste("Value of i is ", i))
    print(paste("Value of j is ", j))
    print(paste("set 1 index for list", i, "at position", j, "is", set_1_index[[i]][j]))
    print(workout_exercises[[i]])
    print(glimpse(set_data_df[[i]]))
    #set_data_df[set_1_index[[i]][j], 4] <- workout_exercises[[i]][j]
  }
}
set_data_df
#THIS IS AN INDEX FOR THE ROWS THAT CONTAIN SET 1. THE INDEX CAPTURES WHEN A NEW EXERCISE BEGAN AND WILL BE USED
#IN THE NEXT STEP TO ASSIGN EXERCISES TO THE APPROPRIATE SETS, REPS AND WEIGHT
#set_1_index <- which(set_data_df$set_number == "SET 1")

#workout_exercises <- workout_exercises[[1]]

for (i in 1:length(set_1_index)) {
  set_data_df[set_1_index[i], 1] <- workout_exercises[i]
}

set_data_df <- set_data_df %>%
  fill(workout_exercises, .direction = "down")

set_data_df

#NOT WORKING BECAUSE ONLY A SINGLE ROW FOR SET 4 BUT 2 EXERCISES. CODE CREATES A NEW ROW FOR SET 4 WHICH IS NOT CORRECT
test <- set_data_df %>%
  map(~bind_cols(.x, workout_exercises))

#IDEA: CAN LOOK TO REPLICATE THE OTHER VECTORS TO THE NUMBER OF SETS IN THE set_data_df AND THEN BIND ROWS
#CAN ALSO LOOK TO UPDATE VALUES IN DATAFRAME ON EVERY nth OCCURENCE OF SET 1.
#CAN ALSO LOOP AND EXTRACT ALL SETS BUT I THINK THIS IS THE SAME AS THE SPLIT

test

df_list <- vector(mode = "list",
                  length = length(workout_titles))

for (i in 1:length(workout_titles)) {
  df_list[[i]] <- data.frame(workout_date[[i]], 
                             workout_titles[[i]], 
                             workout_number[[i]],
                             workout_exercises[[i]], 
                             set_data_df[[i]])
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
  bind_rows() %>%
  filter(weight != 0,
         reps != 0)

#APPEND DATA TO CSV
write_csv(x = output_data,
          path = output_file,
          append = TRUE)

#REMOVING WORKOUT FILES. FILES HAVE BEEN PROCESSED THERE IS NO NEED TO KEEP OVER FILES.
file.remove(workout_files)


