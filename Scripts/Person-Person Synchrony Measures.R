# The results of this code can be found in the One Drive folder
# for the BDS project under the names "sync_results_parent_child_idea_1.rds",
# "sync_results_parent_child_idea_2.rds", and "sync_results_parent_child_idea_3.rds"
# They will also be available as csv files.

# 0. Set up workspace
rm(list = ls())
library(tidyverse)

# 1. View Data 
# This part of the code needs to be adapted to each individual computer.
# The data that is supposed to be loaded can be found in the OneDrive under
# Main Study -> Data -> BDS Project -> full_dataset_synchrony.rds
# This dataset was created in the file "Creating Dataset for Person-Person Synchrony.R"
data <- readRDS("~/Downloads/full_dataset_synchrony.rds")

# View the data to make sure everything loaded as expected
names(data)
head(data)

# data set for the child-performer synchrony # Not used in this code
#data_concert <- data %>%
#  rename("Condition" = "Condition.x") %>%
#  filter(Condition == "Concert")

# Data that includes additional information such as condition, file name, and reviewers
overview_valid_data <- read_delim("~/Downloads/overview_valid_data.csv", delim = ";")

# overview valid data had an unexpected number of rows, so I reduced it to only unique rows
overview_valid_data <- distinct(overview_valid_data)

# The next code creates a table that has information such as session, date, condition, etc. 
# This will be used to add that information to the parent-child (pac) syncrony estimates

additional_information_pac <- overview_valid_data[, 1:9] %>%
  filter(Participant == "Parent" | Participant == "Child") %>%
  pivot_wider(
    id_cols = c(`ParticipantID (in survey)`, Wave, Date, `Time/Session`, Condition),          # columns that are identical and should stay
    names_from = Participant,                                                                 # which column differentiates the rows
    values_from = c(`Sensor (last 4 digits)`, `File name of Cleaned ECG data`, Reviewer),     # the columns that vary by person
    names_sep = "_"
  )  %>%
  rename(dyad_id = `ParticipantID (in survey)`)

# ------------------------------------------------------------------------------
# 2. IDEA 1 — Windowed Cross-Correlation (Axis-wise, then Combined)

# Windowed cross-correlation is a way of measuring how strongly two time-varying
# signals are related to each other over time, while allowing that relationship
# to change instead of assuming it is constant. At its core, cross-correlation
# answers the question “if one signal goes up or down, does the other do something
# similar, and possibly a little earlier or later?” Windowing adds the idea that
# you only ask this question over short time segments, then slide that segment
# forward, so you can see when coordination appears, disappears, or changes direction.

# Source
# Georgescu, A.L., Koeroglu, S., Hamilton, A.F. et al. 
# Reduced nonverbal interpersonal synchrony in autism spectrum disorder independent of partner diagnosis:
# a motion energy study. Molecular Autism 11, 11 (2020). https://doi.org/10.1186/s13229-019-0305-1

library(zoo) # for rolling windows

# Pick any dyad and sensor to estimate dt
example_dyad <- data %>%
  filter(dyad_id == unique(data$dyad_id)[1],
         sensor == unique(sensor)[1]) %>%
  arrange(time_utc_ms)

# Make sure timestamps are numeric
timestamps <- as.numeric(example_dyad$time_utc_ms)

# Difference in ms
dt <- diff(timestamps)

# Sampling rate in Hz (samples per second)
sampRate <- as.numeric(round(1000 / median(dt)))

# Confirm it's numeric
print(sampRate)

# Windowed cross-correlation
run_wcc <- function(a, b, sampRate, win = 30, max_lag = 5) {
  
  # Ensure inputs are numeric vectors and sample rate is numeric
  a <- as.numeric(a)
  b <- as.numeric(b)
  sampRate <- as.numeric(sampRate)
  
  # Return NA if sample rate is missing or invalid
  if (is.na(sampRate) || sampRate <= 0) return(NA_real_)
  
  # Use the shortest signal length to avoid index overflow
  n <- min(length(a), length(b))
  if (n == 0) return(NA_real_)
  
  # Convert window length (seconds) to samples
  win_size <- win * sampRate
  
  # Set window increment to 1 second (in samples)
  inc <- sampRate
  
  # Guard against sample rates that are too low or windows too large
  if (win_size >= n || inc <= 0) return(NA_real_)
  
  # Convert maximum lag (seconds) to samples
  lag_samp <- max_lag * sampRate
  
  # Ensure lag does not exceed window size
  if (lag_samp >= win_size) lag_samp <- win_size - 1
  
  # Initialize vector to store maximum correlations per window
  corrs <- numeric(0)
  
  # Slide window across the signals
  for (start in seq(1, n - win_size, by = inc)) {
    
    # Indices for the current window
    idx <- start:(start + win_size - 1)
    
    # Extract windowed segments from both signals
    x_win <- a[idx]
    y_win <- b[idx]
    
    # Compute cross-correlation within the window
    cc <- ccf(x_win, y_win, lag.max = lag_samp, plot = FALSE)
    
    # Store the maximum absolute correlation value
    corrs <- c(corrs, max(abs(cc$acf), na.rm = TRUE))
  }
  
  # Return NA if no correlations were computed
  if (length(corrs) == 0) return(NA_real_)
  
  # Return the mean of windowed maximum correlations
  mean(corrs, na.rm = TRUE)
}


# Dyad-level synchrony or child-performer level synchrony.
sync_results_1 <- data %>%
  
  # Group data by dyad (pair of participants)
  group_by(dyad_id) %>%
  
  # Apply a custom function to each dyad group
  group_modify(~ {
    
    # Identify the unique sensors present in the dyad
    sensors <- unique(.x$sensor)
    
    # If the dyad does not contain exactly two sensors, return NA results
    if (length(sensors) != 2) {
      return(tibble(sync_x = NA, sync_y = NA, sync_z = NA, sync_overall = NA))
    }
    
    # Extract and time-sort data for the first and second sensor
    s1 <- filter(.x, sensor == sensors[1]) %>% arrange(time_utc_ms)
    s2 <- filter(.x, sensor == sensors[2]) %>% arrange(time_utc_ms)
    
    # Compute windowed cross-correlation synchrony for all axes accelerations
    sync_x <- run_wcc(s1$fx, s2$fx, sampRate)
    sync_y <- run_wcc(s1$fy, s2$fy, sampRate)
    sync_z <- run_wcc(s1$fz, s2$fz, sampRate)
    
    # Return synchrony measures for the dyad
    tibble(
      sync_x = sync_x,
      sync_y = sync_y,
      sync_z = sync_z,
      
      # Compute overall synchrony as the mean across axes
      sync_overall = mean(c(sync_x, sync_y, sync_z), na.rm = TRUE)
    )
  }) %>%
  # Remove grouping structure from the final result
  ungroup()

# Add additional information
sync_results_1 <- sync_results_1 %>%
  left_join(additional_information_pac, by = "dyad_id")

sync_results_1

# Make sure to change the name of the file based on which synchrony was computed
saveRDS(sync_results_1, "sync_results_parent_child_idea_1.rds")
# ------------------------------------------------------------------------------
# IDEA 2 — Cross-Correlation Function per Axis (Leader–Follower & Strength)

# A cross-correlation function per axis is usually computed on the entire time series 
# for each axis (x, y, z) separately. For a given axis, you compute correlations at 
# a range of lags and obtain one curve. From that curve, you typically extract two numbers:
# the maximum absolute correlation, which is interpreted as coordination strength,
# and the lag at which that maximum occurs, which is interpreted as leader–follower 
# direction and delay. If the peak is at lag zero, the movements are simultaneous;
# if it is at a positive lag, one person tends to lead; if it is negative, the other leads.

# Source
# Hale, J., Ward, J. A., Buccheri, F., Oliver, D., & Hamilton, A. F. D. C. (2020).
# Are you on my wavelength? Interpersonal coordination in dyadic conversations.
# Journal of nonverbal behavior, 44(1), 63-83. https://doi.org/10.1007/s10919-019-00320-3

# ccf function
ccf_sync <- function(a, b, max_lag = 100) {
  
  # Coerce inputs to numeric vectors
  a <- as.numeric(a)
  b <- as.numeric(b)
  
  # Trim both signals to the same length
  n <- min(length(a), length(b))
  a <- a[1:n]
  b <- b[1:n]
  
  # Identify indices where both signals have non-missing values
  ok <- complete.cases(a, b)
  a <- a[ok]
  b <- b[ok]
  
  # Bail out safely if there are not enough observations for correlation
  if (length(a) < 2) {
    return(list(strength = NA, lag = NA))
  }
  
  # Compute cross-correlation function up to the specified maximum lag
  cc <- ccf(a, b, lag.max = max_lag, plot = FALSE)
  
  # Find the index of the maximum absolute correlation value
  idx <- which.max(abs(cc$acf))
  
  # Return synchrony strength and the corresponding lag
  list(
    strength = abs(cc$acf[idx]),
    lag      = cc$lag[idx]
  )
}

# Compute the dyad level synchrony
sync_results_2 <- data %>%
  
  # Group observations by dyad (pair of sensors/participants)
  group_by(dyad_id) %>%
  
  # Apply a custom synchrony computation to each dyad
  group_modify(~ {
    
    # Identify the unique sensors present in the dyad
    sensors <- unique(.x$sensor)
    
    # If the dyad does not contain exactly two sensors, return NA results
    if (length(sensors) != 2) {
      return(tibble(
        sensor_1 = NA,
        sensor_2 = NA,
        x_strength = NA, x_lag = NA,
        y_strength = NA, y_lag = NA,
        z_strength = NA, z_lag = NA,
        overall_strength = NA
      ))
    }
    
    # Define sensor 1 and sensor 2 explicitly for consistent ordering
    sensor_1 <- sensors[1]
    sensor_2 <- sensors[2]
    
    # Extract data corresponding to sensor 1 and 2
    s1 <- filter(.x, sensor == sensor_1)
    s2 <- filter(.x, sensor == sensor_2)
    
    # Compute cross-correlation synchrony for all the axes
    x <- ccf_sync(s1$fx, s2$fx)
    y <- ccf_sync(s1$fy, s2$fy)
    z <- ccf_sync(s1$fz, s2$fz)
    
    # Return dyad-level synchrony metrics across all axes
    tibble(
      sensor_1 = sensor_1,
      sensor_2 = sensor_2,
      
      # Strength and lag for the axes synchrony
      x_strength = x$strength,
      x_lag      = x$lag,
      
      y_strength = y$strength,
      y_lag      = y$lag,
      
      z_strength = z$strength,
      z_lag      = z$lag,
      
      # Overall synchrony strength averaged across axes
      overall_strength = mean(
        c(x$strength, y$strength, z$strength),
        na.rm = TRUE
      )
    )
  }) %>%
  # Remove grouping structure from the final output
  ungroup()


# Result, join with additional information
sync_results_2 <- sync_results_2 %>%
  left_join(additional_information_pac, by = "dyad_id")

# Save the results
saveRDS(sync_results_2, "sync_results_parent_child_idea_2.rds")

# ------------------------------------------------------------------------------
# IDEA 3 — Cross-Recurrence Quantification Analysis (CRQA) per Axis

# The code performs CRQA separately for x, y, and z movement trajectories.
# It computes recurrence rate (RR), the proportion of times both systems occupy similar states.
# Synchrony is not always linear; people may coordinate patterns, not amplitudes.
# CRQA captures shared dynamics, even if movements differ in scale or timing.
# Axis-wise CRQA respects the multidimensional nature of movement.
# Averaging recurrence rates yields a summary of global dyadic coupling.

# Source
# Coco, M. I., & Dale, R. (2014). Cross-recurrence quantification analysis of
# categorical and continuous time series: an R package. Frontiers in psychology, 5, 510.
# https://doi.org/10.3389/fpsyg.2014.00510

# This code created memory issues doing one run. Closing R and restarting my
# laptop solved this issue for me. 
library(crqa)

# Estimate sampling rate
example_rows <- data %>%
  arrange(time_utc_ms) %>%
  slice(1:500)

dt <- diff(as.numeric(example_rows$time_utc_ms))
sampRate <- as.numeric(round(1000 / median(dt)))

# Windowed CRQA function
run_crqa_windowed <- function(a, b,
                              sampRate,
                              win_sec  = 5,
                              step_sec = 5,
                              radius   = 0.1,
                              delay    = 1,
                              embed    = 1) {
  
  # Coerce input signals to numeric vectors
  a <- as.numeric(a)
  b <- as.numeric(b)
  
  # Trim both signals to the same length
  n <- min(length(a), length(b))
  a <- a[1:n]
  b <- b[1:n]
  
  # Convert window and step sizes from seconds to samples
  win  <- win_sec  * sampRate
  step <- step_sec * sampRate
  
  # Return NA if the signal is shorter than one analysis window
  if (n < win) return(NA_real_)
  
  # Precompute the number of windows for memory efficiency
  win_starts <- seq(1, n - win, by = step)
  rr_vals <- numeric(length(win_starts))
  
  k <- 1
  # Slide a window across the signals
  for (start in win_starts) {
    
    # Extract windowed segments from both signals
    aw <- a[start:(start + win - 1)]
    bw <- b[start:(start + win - 1)]
    
    # Run cross-recurrence quantification analysis on the window
    res <- crqa(
      ts1 = aw,
      ts2 = bw,
      delay = delay,          # Time delay for phase-space reconstruction
      embed = embed,          # Embedding dimension
      radius = radius,        # Recurrence threshold
      normalize = 0,          # Do not normalize time series
      rescale = 0,            # Do not rescale distance matrix
      mindiagline = 2,        # Minimum diagonal line length
      minvertline = 2,        # Minimum vertical line length
      tw = 0,                 # No Theiler window
      datatype = "continuous",
      metric = "euclidean"    # Distance metric for recurrence
    )
    
    # Store the recurrence rate (RR) for the current window
    rr_vals[k] <- res$RR
    k <- k + 1
  }
  
  # Return the mean recurrence rate across all windows
  mean(rr_vals, na.rm = TRUE)
}

# Dyad-level CRQA with progress bar

# Reduce memory by keeping only the needed columns
data_small <- data %>%
  select(dyad_id, sensor, fx, fy, fz)

# Split data by dyad once (memory safe)
data_by_dyad <- split(data_small, data_small$dyad_id)
dyads <- names(data_by_dyad)
n_dyads <- length(dyads)

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = n_dyads, style = 3)

# Preallocate list for CRQA results
crqa_results <- vector("list", n_dyads)

# Loop over dyads
for (i in seq_along(data_by_dyad)) {
  
  dyad_data <- data_by_dyad[[i]]
  sensors <- unique(dyad_data$sensor)
  
  # Skip dyads that don't have exactly two sensors
  if (length(sensors) != 2) {
    crqa_results[[i]] <- tibble(
      dyad_id = dyads[i],
      sensor_1 = NA,
      sensor_2 = NA,
      rr_x = NA,
      rr_y = NA,
      rr_z = NA,
      rr_overall = NA
    )
    setTxtProgressBar(pb, i)
    next
  }
  
  # Assign sensors explicitly
  sensor_1 <- sensors[1]
  sensor_2 <- sensors[2]
  
  # Extract data for each sensor
  s1 <- dyad_data[dyad_data$sensor == sensor_1, ]
  s2 <- dyad_data[dyad_data$sensor == sensor_2, ]
  
  # Compute windowed CRQA for x, y, z axes
  rr_x <- run_crqa_windowed(s1$fx, s2$fx, sampRate)
  rr_y <- run_crqa_windowed(s1$fy, s2$fy, sampRate)
  rr_z <- run_crqa_windowed(s1$fz, s2$fz, sampRate)
  
  # Save results for this dyad
  crqa_results[[i]] <- tibble(
    dyad_id = dyads[i],
    sensor_1 = sensor_1,
    sensor_2 = sensor_2,
    rr_x = rr_x,
    rr_y = rr_y,
    rr_z = rr_z,
    rr_overall = mean(c(rr_x, rr_y, rr_z), na.rm = TRUE)
  )
  
  # Clean up memory and update progress bar
  gc(FALSE)
  setTxtProgressBar(pb, i)
}

# Combine list of tibbles into a single tibble
crqa_results <- bind_rows(crqa_results)

# Add any additional information
crqa_results <- crqa_results %>%
  left_join(additional_information_pac, by = "dyad_id")

# save the results
saveRDS(crqa_results, "sync_results_parent_child_idea_3.rds")

