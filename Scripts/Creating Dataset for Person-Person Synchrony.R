# 0. Set up workspace
rm(list = ls())
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(data.table)
library(tidyr)

# 1. Load and View Data 

# This part of the code needs to be adapted to each individual computer.
# The data that is supposed to be loaded can be found in the OneDrive under
# Main Study -> Data -> BDS Project -> acc_all_data.rds

full_data_IMU <- readRDS("~/Downloads/acc_all_data.rds")

# To get an overview of the data.
# Using View(full_data_IMU) may cause R to crash due to RAM limitations.
names(full_data_IMU)
head(full_data_IMU)

# Also needs individual adaptation.
# overview_valid_data.csv refers to the file Main Study -> Data -> Physiological Data
# -> ECG Data Overview.xlsx -> The first excel sheet of that file.
# I renamed it to overview_valid_data.csv.
overview_valid_data <- read_delim("~/Downloads/overview_valid_data.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

# remove columns and rows with NAs
overview_valid_data <- overview_valid_data[, 1:9]
overview_valid_data <- overview_valid_data[complete.cases(overview_valid_data), ]

names(overview_valid_data)
head(overview_valid_data)

# 2. Load the Files with the markers
# The purpose of loading this data frame is to remove all the data in our large data set
# with the movement data that is outside the start and stop times.
# Can be found under Main Study -> Data -> Physiological Data -> Marker Breakdown.xlsx -> 
# All sheets from that file

playtime_april_1_s1 <- read_delim("~/Downloads/Marker Breakdown(Apr 1 - S1 - Playtime).csv", delim = ";")
playtime_april_1_s2 <- read_delim("~/Downloads/Marker Breakdown(Apr 1 - S2 - Playtime).csv", delim = ";")
playtime_april_3_s1 <- read_delim("~/Downloads/Marker Breakdown(Apr 3 - S1 - Playtime).csv", delim = ";")
playtime_april_3_s2 <- read_delim("~/Downloads/Marker Breakdown(Apr 3 - S2 - Playtime).csv", delim = ";")
playtime_april_5_s1 <- read_delim("~/Downloads/Marker Breakdown(Apr 5 - S1 - Playtime).csv", delim = ";")
playtime_april_5_s2 <- read_delim("~/Downloads/Marker Breakdown(Apr 5 - S2 - Playtime).csv", delim = ";")
playtime_nov_3_dr <- read_delim("~/Downloads/Marker Breakdown(Nov 3 - DR - Playtime).csv", delim = ";")
playtime_nov_5_s1 <- read_delim("~/Downloads/Marker Breakdown(Nov 5 - S1 - Playtime).csv", delim = ";")
playtime_nov_5_s2 <- read_delim("~/Downloads/Marker Breakdown(Nov 5 - S2 - Playtime).csv", delim = ";")
playtime_nov_7_s1 <- read_delim("~/Downloads/Marker Breakdown(Nov 7 - S1 - Playtime).csv", delim = ";")
playtime_nov_7_s2 <- read_delim("~/Downloads/Marker Breakdown(Nov 7 - S2 - Playtime).csv", delim = ";")
playtime_nov_8_s1 <- read_delim("~/Downloads/Marker Breakdown(Nov 8 - S1 - Playtime).csv", delim = ";")

concert_april_2_s1 <- read_delim("~/Downloads/Marker Breakdown(Apr 2 - S1 - Concert).csv", delim = ";")
concert_april_2_s2 <- read_delim("~/Downloads/Marker Breakdown(Apr 2 - S2 - Concert).csv", delim = ";")
concert_april_4_s1 <- read_delim("~/Downloads/Marker Breakdown(Apr 4 - S1 - Concert).csv", delim = ";")
concert_april_4_s2 <- read_delim("~/Downloads/Marker Breakdown(Apr 4 - S2 - Concert).csv", delim = ";")
concert_nov_4_s1 <- read_delim("~/Downloads/Marker Breakdown(Nov 4 - S1 - Concert).csv", delim = ";")
concert_nov_4_s2 <- read_delim("~/Downloads/Marker Breakdown(Nov 4 - S2 - Concert).csv", delim = ";")
concert_nov_6_s1 <- read_delim("~/Downloads/Marker Breakdown(Nov 6 - S1 - Concert).csv", delim = ";")
concert_nov_6_s2 <- read_delim("~/Downloads/Marker Breakdown(Nov 6 - S2 - Concert).csv", delim = ";")
concert_nov_8_s1 <- read_delim("~/Downloads/Marker Breakdown(Nov 8 - S1 - Concert).csv", delim = ";")
concert_nov_8_s2 <- read_delim("~/Downloads/Marker Breakdown(Nov 8 - S2 - Concert).csv", delim = ";")

# create a list for more interpretable and shorter code
list_all <- list(playtime_april_1_s1, playtime_april_1_s2, playtime_april_3_s1,
                 playtime_april_3_s2, playtime_april_5_s1, playtime_april_5_s2,
                 playtime_nov_3_dr, playtime_nov_5_s1, playtime_nov_5_s2,
                 playtime_nov_7_s1, playtime_nov_7_s2, playtime_nov_8_s1,
                 concert_april_2_s1, concert_april_2_s2, concert_april_4_s1,
                 concert_april_4_s2, concert_nov_4_s1, concert_nov_4_s2,
                 concert_nov_6_s1, concert_nov_6_s2, concert_nov_8_s1, concert_nov_8_s2)

# Find the common columns to combine the files to a data frame
common_cols <- Reduce(intersect, lapply(list_all, names))

# Create the data frame, while disregarding all columns that aren't shared by all
# tables

combined_marker_files <- do.call(
  rbind,
  lapply(list_all, function(df) df[, common_cols, drop = FALSE])
)

# Reduce the data frame to its most necessary parts to try and reduce RAM issues.
combined_marker_files <- combined_marker_files[, 1:5] %>%
  filter(Marker == "PLAYTIME_STOP" | Marker == "PLAYTIME_START" | Marker == "CONCERT_STOP" | Marker == "CONCERT_START")

# Check that the data frame looks as expected
names(combined_marker_files)
head(combined_marker_files)

# join overview and marker tables because the marker table does not include 
# information like sensor, id, and session. That info is necessary to later
# line up the start and stop markers correctly to the movement data. 
overview_valid_data <- overview_valid_data %>%
  left_join(
    combined_marker_files,
    by = c("ParticipantID (in survey)" = "ID", 
           "Sensor (last 4 digits)" = "Sensor", 
           "Participant" = "P/C"))

# I renamed the column "ParticipantID (in survey)" because names in camel case
# are easier to use in R
overview_valid_data <- overview_valid_data %>%
  rename(dyad_id = "ParticipantID (in survey)",
         participant_type = Participant)

# Another check to see that the join() worked correctly
names(overview_valid_data)
head(overview_valid_data)

# 2. Adapt the data frames so that key identifying columns have the same type and
# data format

# Detangle "subject path" (date, session, sensor) from the full data file so we 
# can use the info to join tables
full_data_IMU <- full_data_IMU %>%
  mutate(
    date    = str_squish(str_extract(subject_path, "^[^/]+")),
    session = str_squish(str_extract(subject_path, "Session\\s*\\d+")),
    sensor  = str_squish(str_extract(subject_path, "\\d+$"))
  )

# Make sure everything is the right type (int, date, etc.)
overview_valid_data <- overview_valid_data %>%
  mutate(
    date     = str_to_lower(str_squish(Date)),
    session = str_to_lower(str_squish(`Time/Session`)),
    sensor  = as.numeric(`Sensor (last 4 digits)`)
  )

# Normalize the dates in the tables for joining them
full_data_IMU <- full_data_IMU %>%
  mutate(
    # parse "April 1" into a Date object (assume a dummy year, e.g., 2023)
    date_parsed = dmy(paste0(str_extract(date, "\\d+"), "-", str_extract(date, "[a-zA-Z]+"), "-2023")),
    date_norm   = format(date_parsed, "%d.%m"),  # e.g., "01.04"
    session     = str_to_lower(str_squish(session))
  )

overview_valid_data <- overview_valid_data %>%
  mutate(
    # parse "1-apr" into Date object (assume a dummy year again, e.g., 2023)
    date_parsed = dmy(paste0(str_extract(date, "\\d+"), "-", str_extract(date, "[a-zA-Z]+"), "-2023")),
    date_norm   = format(date_parsed, "%d.%m"),
    session     = str_to_lower(str_squish(session))
  )

# Make the sensor variable a character in both tables
full_data_IMU <- full_data_IMU %>%
  mutate(sensor = as.character(sensor))

overview_valid_data <- overview_valid_data %>%
  mutate(sensor = as.character(sensor))

# 3. remove unnecessary data outside of the start and stop markers

# We use tidyr to get start/stop into columns, then convert to data.table
markers_dt <- overview_valid_data %>%
  # Convert Unix seconds to POSIXct
  mutate(timestamp_clean = as.POSIXct(`Unix Timestamp`, origin = "1970-01-01", tz = "UTC")) %>%
  select(date_norm, session, sensor, Marker, timestamp_clean) %>%
  pivot_wider(names_from = Marker, values_from = timestamp_clean) %>%
  mutate(
    start_time = coalesce(PLAYTIME_START, CONCERT_START),
    stop_time  = coalesce(PLAYTIME_STOP, CONCERT_STOP)
  ) %>%
  select(date_norm, session, sensor, start_time, stop_time) %>%
  as.data.table()

# Prepare the Observation (IMU) Table by converting it to a more RAM-friendly option
setDT(full_data_IMU)

# Convert Milliseconds to POSIXct directly in the data.table (saves memory)
full_data_IMU[, obs_time_clean := as.POSIXct(time_utc_ms / 1000, 
                                             origin = "1970-01-01", 
                                             tz = "UTC")]

# Perform the Join
# This filters rows WHILE joining so it never hits the 16GB limit
cleaned_data <- full_data_IMU[markers_dt, 
                              on = .(date_norm, session, sensor, 
                                     obs_time_clean >= start_time, 
                                     obs_time_clean <= stop_time), 
                              nomatch = NULL]

# Clean up names
setnames(cleaned_data, 
         old = c("obs_time_clean", "obs_time_clean.1"), 
         new = c("start_time", "stop_time"))

# only keep necessary columns
# feel free to adapt if you feel like more columns are necessary and or interesting
# Again an attempt to reduce the data size
cleaned_data <- cleaned_data %>%
  select("time_utc_ms", "timestamp_ms", "fx", "fy", "fz", "intensity", "session", "sensor", "date_norm",)

# Preview the result
head(cleaned_data)
names(cleaned_data)

# 4. Join with the overview table so that we have all the data we need for the
# synchrony calculations

# Only select the interesting columns
overview_valid_data <- overview_valid_data %>%
  select("dyad_id", "participant_type", "Condition", "File name of Cleaned ECG data",
         "Reviewer", "session", "sensor", "date_norm")

# Making sure no rows exist twice after all this work on the tables
overview_valid_data <- overview_valid_data %>% distinct()

# Convert to data.table
setDT(cleaned_data)
setDT(overview_valid_data)

# Set composite keys
setkey(cleaned_data, date_norm, sensor, session)
setkey(overview_valid_data, date_norm, sensor, session)

# Left join (cleaned_data keeps all rows)
result <- overview_valid_data[cleaned_data]

saveRDS(result, "full_dataset_synchrony.rds")


