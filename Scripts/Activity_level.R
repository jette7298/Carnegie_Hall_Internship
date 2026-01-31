# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
library(DBI)
library(duckdb)
library(tidyverse)
library(dbplyr)
library(effectsize)
library(signal)

# ------------------------------------------------------------
# 0) Connect + reference DB tables
# ------------------------------------------------------------
con <- dbConnect(duckdb::duckdb(), dbdir = "./data/imu.duckdb") 

dbListTables(con)
acc_data_in_window_db <- tbl(con, "acc_data_in_window_db")
masterdata_db         <- tbl(con, "masterdata_db")

# ------------------------------------------------------------
# 1) Create LOW-PASS filtered acceleration table (DB-backed)
#    - Filtering is done in R (filtfilt)
#    - Never filter across ECG_ParticipantID boundaries
# ------------------------------------------------------------
fs <- 208
cutoff <- 10
nyq <- 0.5 * fs
bf <- butter(4, cutoff / nyq, type = "low")

df_lp <- acc_data_in_window_db %>%
  select(ECG_ParticipantID, timestamp_ms, x, y, z) %>%
  arrange(ECG_ParticipantID, timestamp_ms) %>%
  collect()

df_lp <- df_lp %>%
  group_by(ECG_ParticipantID) %>%
  mutate(
    lx = filtfilt(bf, x),
    ly = filtfilt(bf, y),
    lz = filtfilt(bf, z)
  ) %>%
  ungroup()

DBI::dbExecute(con, "DROP TABLE IF EXISTS acc_data_in_window_lp_db")
DBI::dbWriteTable(
  con,
  name = "acc_data_in_window_lp_db",
  value = df_lp,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 2) Movement pipeline (raw / filtered / low-pass)
#    - Fully DB-backed except for filtering
# ------------------------------------------------------------
run_movement_pipeline <- function(signal = c("filtered", "raw", "lowpass")) {
  signal <- match.arg(signal)
  
  if (signal == "filtered") {
    src_tbl <- "acc_data_in_window_db"
    ax <- "fx"; ay <- "fy"; az <- "fz"
    suffix <- "fxyz"
  } else if (signal == "raw") {
    src_tbl <- "acc_data_in_window_db"
    ax <- "x"; ay <- "y"; az <- "z"
    suffix <- "xyz"
  } else {  # lowpass
    src_tbl <- "acc_data_in_window_lp_db"
    ax <- "lx"; ay <- "ly"; az <- "lz"
    suffix <- "lxyz"
  }
  
  movement_table <- paste0("acc_movement_", suffix, "_db")
  summary_table  <- paste0("acc_movement_summary_", suffix, "_db")
  
  # ----------------------------------------------------------
  # 2.1 Per-row movement (Euclidean norm of successive diffs)
  # ----------------------------------------------------------
  DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", movement_table))
  
  tbl(con, src_tbl) %>%
    group_by(ECG_ParticipantID) %>%
    mutate(
      d1 = !!sym(ax) - lag(!!sym(ax), order_by = timestamp_ms),
      d2 = !!sym(ay) - lag(!!sym(ay), order_by = timestamp_ms),
      d3 = !!sym(az) - lag(!!sym(az), order_by = timestamp_ms),
      movement = sqrt(d1*d1 + d2*d2 + d3*d3)
    ) %>%
    ungroup() %>%
    compute(name = movement_table, temporary = FALSE)
  
  # ----------------------------------------------------------
  # 2.2 Per-participant summary
  # ----------------------------------------------------------
  DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", summary_table))
  
  tbl(con, movement_table) %>%
    dplyr::filter(!is.na(movement)) %>%
    group_by(ECG_ParticipantID) %>%
    dplyr::summarise(
      mean_movement   = mean(movement),
      median_movement = sql("median(movement)"),
      n_samples       = n(),
      .groups = "drop"
    ) %>%
    compute(name = summary_table, temporary = FALSE)
  
  # ----------------------------------------------------------
  # 2.3 Join MASTERDATA + child-only
  # ----------------------------------------------------------
  movement_child_db <- tbl(con, summary_table) %>%
    left_join(
      masterdata_db %>%
        select(ECG_ParticipantID, ParticipantType, Condition_Physio) %>%
        distinct(),
      by = "ECG_ParticipantID"
    ) %>%
    dplyr::filter(ParticipantType == "Child")
  
  
  
  analysis_df <- movement_child_db %>%
    select(Condition_Physio, mean_movement) %>%
    collect()
  
  list(
    desc = movement_child_db %>%
      group_by(Condition_Physio) %>%
      dplyr::summarise(
        n_ids = n(),
        mean_movement = mean(mean_movement),
        sd_movement   = sd(mean_movement),
        .groups = "drop"
      ) %>%
      collect(),
    
    t_test = t.test(mean_movement ~ Condition_Physio, data = analysis_df),
    d      = cohens_d(mean_movement ~ Condition_Physio, data = analysis_df),
    
    analysis_df = analysis_df
  )
}

# ------------------------------------------------------------
# 3) Run all signal variants
# ------------------------------------------------------------
res_fxyz <- run_movement_pipeline("filtered")  # fx / fy / fz
res_xyz  <- run_movement_pipeline("raw")       # x  / y  / z
res_lxyz <- run_movement_pipeline("lowpass")   # lx / ly / lz

res_fxyz$desc; res_fxyz$t_test; res_fxyz$d
res_xyz$desc;  res_xyz$t_test;  res_xyz$d
res_lxyz$desc; res_lxyz$t_test; res_lxyz$d