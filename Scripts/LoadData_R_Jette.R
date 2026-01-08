# -----------------------------
# Libraries
# -----------------------------
library(jsonlite)
library(readr)
library(dplyr)
library(purrr)
library(signal)
library(tibble)

# -----------------------------
# 1) Helper: pick first matching file in a folder
# -----------------------------
pick_file <- function(dir, pattern) {
  list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)[1]
}

# -----------------------------
# 2) Read timemap + markers
# -----------------------------
read_timemap <- function(path) {
  tm <- read_csv(path, show_col_types = FALSE)
  names(tm) <- trimws(names(tm))
  list(
    utc   = tm[["Device UTC (us)"]][1],
    local = tm[["Device local (ms)"]][1]
  )
}

read_markers <- function(path) {
  mk <- read_csv(path, col_names = FALSE, show_col_types = FALSE)
  
  # make marker names unique: A, A2, A3, ...
  names_raw <- trimws(mk$X2)
  counts <- list()
  names_new <- character(length(names_raw))
  for (i in seq_along(names_raw)) {
    nm <- names_raw[i]
    if (!is.null(counts[[nm]])) {
      counts[[nm]] <- counts[[nm]] + 1L
      names_new[i] <- paste0(nm, counts[[nm]])
    } else {
      counts[[nm]] <- 1L
      names_new[i] <- nm
    }
  }
  
  list(
    name = names_new,
    time = mk$X1 * 1000  # s -> ms
  )
}

# -----------------------------
# 3) Read JSON samples
# -----------------------------
read_data_json <- function(path) {
  j <- fromJSON(path, simplifyVector = FALSE)
  samples <- j$Samples
  meas <- keep(samples, ~ !is.null(.x$MeasIMU9)) |> map("MeasIMU9")
  map(meas, ~ list(Timestamp = .x$Timestamp, ArrayAcc = .x$ArrayAcc))
}

# -----------------------------
# 4) Filtering + signal creation (fast)
# -----------------------------
filt_norm_signal <- function(x, fs) {
  nyq <- 0.5 * fs
  x <- filtfilt(butter(4, 0.5 / nyq, type = "high"), x)  # remove gravity
  x <- filtfilt(butter(4, 10 / nyq,  type = "low"),  x)  # remove noise
  x
}

create_signal <- function(data, fs = 208, n_samples = 8) {
  period <- 1000 / fs
  n_total <- length(data) * n_samples
  
  x <- numeric(n_total); y <- numeric(n_total); z <- numeric(n_total)
  t <- numeric(n_total)
  
  idx <- 1L
  for (dp in data) {
    ts0 <- dp$Timestamp
    samples <- dp$ArrayAcc
    n <- length(samples)
    
    for (i in seq_len(n)) {
      s <- samples[[i]]
      x[idx] <- s$x
      y[idx] <- s$y
      z[idx] <- s$z
      t[idx] <- ts0 + (i - 1) * period
      idx <- idx + 1L
    }
  }
  
  if (idx <= n_total) {
    keep <- seq_len(idx - 1L)
    x <- x[keep]; y <- y[keep]; z <- z[keep]; t <- t[keep]
  }
  
  unfiltered <- sqrt(x^2 + y^2 + z^2)
  
  fx <- filt_norm_signal(x, fs)
  fy <- filt_norm_signal(y, fs)
  fz <- filt_norm_signal(z, fs)
  
  list(
    t = t,
    x = x, y = y, z = z,
    fx = fx, fy = fy, fz = fz,
    unfiltered = unfiltered,
    intensity = sqrt(fx^2 + fy^2 + fz^2)
  )
}

local2utc <- function(timestamp_ms, local_ms, utc_us) {
  timestamp_ms - local_ms + (utc_us / 1000)
}

# -----------------------------
# 5) Load ONE session folder
#    - JSON + timemap always from raw
#    - marker: use corrected if available, else raw
# -----------------------------
load_acc_session <- function(raw_root, subject_rel_path,
                             corrected_marker_root = NULL,
                             fs = 208, n_samples = 8) {
  
  raw_dir <- file.path(raw_root, subject_rel_path)
  corr_dir <- if (!is.null(corrected_marker_root)) file.path(corrected_marker_root, subject_rel_path) else NULL
  
  json_file   <- pick_file(raw_dir, "\\.json$")
  timemap_file <- pick_file(raw_dir, "timemap.*\\.txt$")
  
  marker_file <- if (!is.null(corr_dir) && dir.exists(corr_dir)) {
    pick_file(corr_dir, "marker.*\\.txt$")
  } else {
    pick_file(raw_dir, "marker.*\\.txt$")
  }
  
  data <- read_data_json(json_file)
  tm <- read_timemap(timemap_file)
  events <- read_markers(marker_file)
  
  sig <- create_signal(data, fs = fs, n_samples = n_samples)
  time_utc <- local2utc(sig$t, tm$local, tm$utc)
  
  df <- tibble(
    time_utc_ms = time_utc,
    timestamp_ms = sig$t,
    x = sig$x, y = sig$y, z = sig$z,
    fx = sig$fx, fy = sig$fy, fz = sig$fz,
    intensity = sig$intensity,
    intensity_unfiltered = sig$unfiltered,
    subject_path = factor(subject_rel_path)
  )
  
  list(data = df, events = events)
}

# -----------------------------
# 6) Batch load ONLY the folders in `to_load`
# -----------------------------
#time keeping
t0 <- Sys.time()


load_from_to_load <- function(to_load,
                              raw_root = "data",
                              corrected_marker_root = file.path("data", "_Corrected Marker Files"),
                              fs = 208, n_samples = 8) {
  
  out <- vector("list", length(to_load))
  
  for (i in seq_along(to_load)) {
    p <- to_load[i]
    message(sprintf("[%d/%d] %s", i, length(to_load), p))
    
    out[[i]] <- load_acc_session(
      raw_root = raw_root,
      subject_rel_path = p,
      corrected_marker_root = corrected_marker_root,
      fs = fs,
      n_samples = n_samples
    )
  }
  
  acc_data <- bind_rows(lapply(out, `[[`, "data"))
  acc_data$subject_path <- factor(acc_data$subject_path)
  
  list(all_data = acc_data, sessions = out)
}

#time keeping
Sys.time() - t0

# -----------------------------
# Example
# -----------------------------
to_load <- readRDS("data/to_load_subject_paths.rds")
all <- load_from_to_load(to_load)
all$all_data

# -----------------------------
# save data
# -----------------------------
saveRDS(all$all_data, "data/acc_all_data.rds")

