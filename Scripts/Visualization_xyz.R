# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
library(DBI)
library(duckdb)
library(tidyverse)   # dplyr, ggplot2, tidyr
library(dbplyr)
library(plotly)

# ------------------------------------------------------------
# 0) Connect + reference DB tables
# ------------------------------------------------------------
con <- dbConnect(duckdb::duckdb(), dbdir = "./data/imu.duckdb")

dbListTables(con)

acc_data_in_window_db <- tbl(con, "acc_data_in_window_db")
masterdata_db         <- tbl(con, "masterdata_db")

# ------------------------------------------------------------
# A) Sample 10 participants (ECG_ParticipantID) – RAW x/y/z
# ------------------------------------------------------------

# 1) Sample 10 participant IDs directly in DuckDB
random_ids <- dbGetQuery(con, "
  SELECT DISTINCT ECG_ParticipantID
  FROM acc_data_in_window_db
  USING SAMPLE 10
")

# 2) Write sampled IDs back for DB-side joins
dbWriteTable(con, "random_ids_db", random_ids, overwrite = TRUE)
random_ids_db <- tbl(con, "random_ids_db")

# 3) Subset raw acceleration data
multi_users_db <- acc_data_in_window_db %>%
  semi_join(random_ids_db, by = "ECG_ParticipantID") %>%
  select(ECG_ParticipantID, timestamp_ms, x, y, z)

# 4) Materialize (small table → fast plotting)
dbExecute(con, "DROP TABLE IF EXISTS acc_multi_users_db")

multi_users_db %>%
  compute("acc_multi_users_db", temporary = FALSE)

multi_users_df <- tbl(con, "acc_multi_users_db") %>% collect()

multi_users_long <- multi_users_df %>%
  pivot_longer(cols = c(x, y, z),
               names_to = "axis",
               values_to = "acc")

# -------------------------
# A1) Histograms (x, y, z)
# -------------------------
ggplot(multi_users_long, aes(x = acc, fill = axis)) +
  geom_histogram(bins = 200, alpha = 0.25, position = "identity") +
  facet_wrap(~ ECG_ParticipantID, ncol = 4, scales = "free_y") +
  coord_cartesian(xlim = c(-18, 10)) +
  theme_minimal()

# -------------------------
# A2) Movement energy over time (RAW magnitude)
# -------------------------
multi_users_mag <- tbl(con, "acc_multi_users_db") %>%
  mutate(magnitude = sqrt(x^2 + y^2 + z^2)) %>%
  collect()

p <- ggplot(
  multi_users_mag,
  aes(x = timestamp_ms, y = magnitude, group = ECG_ParticipantID)
) +
  geom_line(alpha = 0.6) +
  facet_wrap(~ ECG_ParticipantID, scales = "free_y") +
  theme_minimal()

print(p)


# -------------------------
# A3) 3D trajectory for ONE participant (raw x/y/z)
# -------------------------
one_id <- random_ids$ECG_ParticipantID[1]

one_df <- acc_data_in_window_db %>%
  dplyr::filter(ECG_ParticipantID == one_id) %>%
  arrange(timestamp_ms) %>%
  transmute(
    ECG_ParticipantID,
    timestamp_ms,
    x, y, z
  ) %>%
  head(5000) %>%   # translates to LIMIT 5000
  collect() %>%
  group_by(ECG_ParticipantID) %>%
  arrange(timestamp_ms) %>%
  mutate(t = (timestamp_ms - first(timestamp_ms)) / 1000) %>%
  ungroup()


plot_ly(
  one_df,
  x = ~x, y = ~y, z = ~z,
  type = "scatter3d",
  mode = "lines",
  text = ~paste0("t=", round(t, 2), "s"),
  hoverinfo = "text"
) %>%
  layout(
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "z")
    ),
    title = paste0("3D acceleration trajectory (raw x/y/z) — ", one_id)
  )

# ------------------------------------------------------------
# B) 3D trajectories for ONE dyad (Parent + Child)
# ------------------------------------------------------------

# 1) Pick one dyad with both Parent and Child
dyad_id <- masterdata_db %>%
  filter(!is.na(ParticipantID)) %>%
  group_by(ParticipantID) %>%
  summarise(n_types = n_distinct(ParticipantType), .groups = "drop") %>%
  filter(n_types >= 2) %>%
  arrange(ParticipantID) %>%  # optional: deterministic
  head(1) %>%                 # DB-safe (LIMIT 1)
  collect() %>%
  pull(ParticipantID)



# 2) Get participant IDs for that dyad
dyad_ids <- masterdata_db %>%
  filter(
    ParticipantID == dyad_id,
    ParticipantType %in% c("Parent", "Child")
  ) %>%
  select(ECG_ParticipantID, ParticipantType) %>%
  distinct() %>%
  collect()


# 3) Pull RAW trajectories
traj_df <- acc_data_in_window_db %>%
  filter(ECG_ParticipantID %in% dyad_ids$ECG_ParticipantID) %>%
  select(ECG_ParticipantID, timestamp_ms, x, y, z) %>%
  collect() %>%
  group_by(ECG_ParticipantID) %>%
  arrange(timestamp_ms) %>%
  mutate(t = (timestamp_ms - first(timestamp_ms)) / 1000) %>%
  slice_head(n = 4000) %>%
  ungroup() %>%
  left_join(dyad_ids, by = "ECG_ParticipantID")

# 4) One 3D plot, colored by ParticipantType
plot_ly(
  traj_df,
  x = ~x, y = ~y, z = ~z,
  color = ~ParticipantType,
  type = "scatter3d",
  mode = "lines",
  text = ~paste0(ParticipantType, "<br>t=", round(t, 2), "s"),
  hoverinfo = "text"
) %>%
  layout(
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "z")
    ),
    title = paste0("3D acceleration trajectories (raw) — DyadID = ", dyad_id)
  )

# ------------------------------------------------------------
# C) Histogram for ONE random participant (raw x/y/z)
# ------------------------------------------------------------
random_one_id <- dbGetQuery(con, "
  SELECT DISTINCT ECG_ParticipantID
  FROM acc_data_in_window_db
  USING SAMPLE 1
")$ECG_ParticipantID[1]

one_obs <- acc_data_in_window_db %>%
  filter(ECG_ParticipantID == random_one_id) %>%
  select(ECG_ParticipantID, x, y, z) %>%
  collect()

one_obs_long <- one_obs %>%
  pivot_longer(cols = c(x, y, z),
               names_to = "axis",
               values_to = "acc")

ggplot(one_obs_long, aes(x = acc, fill = axis)) +
  geom_histogram(bins = 200, alpha = 0.25, position = "identity") +
  facet_wrap(~ ECG_ParticipantID, scales = "free_y") +
  coord_cartesian(xlim = c(-18, 10)) +
  theme_minimal()
