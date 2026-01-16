library(dplyr)
library(dbplyr)

# 1) Per-row movement table
dbExecute(con, "DROP TABLE IF EXISTS acc_movement")

acc_movement <- tbl(con, "acc_data_clean") %>%
  group_by(key) %>%  # IMPORTANT: group BEFORE lag()
  mutate(
    dfx = fx - lag(fx, order_by = timestamp_ms),
    dfy = fy - lag(fy, order_by = timestamp_ms),
    dfz = fz - lag(fz, order_by = timestamp_ms),
    movement = sqrt(dfx*dfx + dfy*dfy + dfz*dfz)
  ) %>%
  ungroup() %>%
  compute(name = "acc_movement", temporary = FALSE)

# 2) Per-key summary table (mean + median)
dbExecute(con, "DROP TABLE IF EXISTS acc_movement_summary")

acc_movement_summary <- tbl(con, "acc_movement") %>%
  filter(!is.na(movement)) %>%   # drop first sample per key
  group_by(key) %>%
  summarise(
    mean_movement   = mean(movement),
    median_movement = sql("median(movement)"),
    n_samples       = n(),
    .groups = "drop"
  ) %>%
  compute(name = "acc_movement_summary", temporary = FALSE)

# 3) Join with metadata
movement_tbl <- tbl(con, "acc_movement_summary")
meta_tbl     <- tbl(con, "metadata")

movement_meta <- movement_tbl %>%
  left_join(meta_tbl, by = c("key" = "valid_key"))

# 4) Descriptives by condition
desc_condition <- movement_meta %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    mean_mean_movement   = mean(mean_movement),
    sd_mean_movement     = sd(mean_movement),
    median_mean_movement = sql("median(mean_movement)"),
    mean_median_movement = mean(median_movement),
    .groups = "drop"
  ) %>%
  collect()

desc_condition


# Descriptives by participant
desc_participant <- movement_meta %>%
  group_by(participant) %>%
  summarise(
    n = n(),
    mean_mean_movement = mean(mean_movement),
    sd_mean_movement   = sd(mean_movement),
    .groups = "drop"
  ) %>%
  collect()

desc_participant

# t-tests
analysis_df <- movement_meta %>%
  select(participant, condition, mean_movement, median_movement) %>%
  collect()

t_condition   <- t.test(mean_movement ~ condition, data = analysis_df)
t_participant <- t.test(mean_movement ~ participant, data = analysis_df)

t_condition
t_participant

# effect sizes
library(effectsize)

# Condition effect
d_condition <- cohens_d(mean_movement ~ condition, data = analysis_df)
d_condition

# Participant effect
d_participant <- cohens_d(mean_movement ~ participant, data = analysis_df)
d_participant

# percent difference between conditions
means_by_condition <- analysis_df %>%
  group_by(condition) %>%
  summarise(mean_movement = mean(mean_movement))

means_by_condition

m1 <- means_by_condition$mean_movement[1]
m2 <- means_by_condition$mean_movement[2]

percent_diff <- (m2 - m1) / m1 * 100
percent_diff


