library(tidyverse)

data = readRDS("C:/Users/mfhid/OneDrive/Documenten/Carnegie_Hall_Internship/acc_all_data.rds")

# Check NA's
sum(is.na(data)) # Contains 0 NA's

# View head
h = head(data)
h

#######################
#DATA PRE-PROCESSING
#######################

# Transform subject_path to character
data$subject_path <- as.character(data$subject_path)

# Split subject_path variable (takes very long to load)
#data = data |>
# separate(subject_path,
#           into = c("date", "session", "sensor"),
#           sep = "/")

# Split dataset into subsets according to dates
data = data |>
  mutate(date = str_split(subject_path, "/", simplify = TRUE)[,1]) # |>
#  mutate(session = str_split(subject_path, "/", simplify = TRUE)[,2]) |>
#  mutate(sensor = str_split(subject_path, "/", simplify = TRUE)[,3])

data = data |>
  mutate(session = str_split(subject_path, "/", simplify = TRUE)[,2])

data = data |> 
  mutate(sensor = str_split(subject_path, "/", simplify = TRUE)[,3])

# Save dataset to 
saveRDS(data, file = "acc_all_data_subjectpath_splitted.rds")

unique(data$date)

april1 = data |>
  filter(date == "April 1")