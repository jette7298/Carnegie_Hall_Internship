#library
library(tidyverse)
library(readxl)

# read filter data
valid <- read_excel("data/ECG Data Overview.xlsx", sheet = 1)
diads <- read_excel("data/ECG Data Overview.xlsx", sheet = 3)

## Preparing diads dataframe
#for diads drop colums 4 to 7, as they are not needed
diads <- diads %>% select(-c(4:7))
#make first row columnn name
colnames(diads) <- diads[1, ]
#drop first row
diads <- diads[-1, ]


#now for valid 
#first the column names
colnames(valid) <- c(
  "wave",
  "participant_id_survey",
  "date",
  "time_session",
  "sensor_last4",
  "participant",
  "condition",
  "cleaned_ecg_filename",
  "correcting_ecg",
  "reviewer",
  "comments_markers",
  "comments_ecg_signal"
)

# now drop every row that is not in diads
valid <- valid %>% dplyr::filter(participant_id_survey %in% diads$ID)

#composite key for filtering later

valid <- valid %>%
  mutate(
    # Convert Date to folder-style "April 1"
    date_folder = paste0(
      format(as.Date(date), "%B"),
      " ",
      as.integer(format(as.Date(date), "%d"))
    ),
    
    # Extract session number (e.g. "1" from "Session 1")
    session_nr = str_extract(time_session, "\\d+"),
    
    # Ensure sensor is character
    sensor_chr = as.character(sensor_last4),
    
    # Composite key
    valid_key = paste(date_folder, session_nr, sensor_chr, sep = "|")
  )


# now save that file
write_csv(valid, "data/valid_sessions.csv")
