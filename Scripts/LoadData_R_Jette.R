#libraries
library(jsonlite)
library(tidyverse)
library(tibble)
library(signal)




#file picking & reading
pick_one <- function(raw_dir, pattern, corrected_dir = NULL) {
  # Prefer corrected if provided and file exists
  if (!is.null(corrected_dir) && dir.exists(corrected_dir)) {
    hits_c <- list.files(corrected_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    if (length(hits_c) > 0) return(hits_c[1])
  }
  hits_r <- list.files(raw_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(hits_r) > 0) return(hits_r[1])
  
  stop(sprintf("Missing file with pattern '%s' in '%s'%s",
               pattern, raw_dir,
               if (!is.null(corrected_dir)) paste0(" or '", corrected_dir, "'") else ""))
}

#Helper: file with corrected timestamp
pick_one2 <- function(primary_dir, pattern, override_dir = NULL) {
  # if override_dir is given, prefer that folder
  if (!is.null(override_dir) && dir.exists(override_dir)) {
    hits_o <- list.files(override_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    if (length(hits_o) > 0) return(hits_o[1])
  }
  
  hits_p <- list.files(primary_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(hits_p) > 0) return(hits_p[1])
  
  stop(sprintf("Missing file pattern '%s' in '%s'%s",
               pattern, primary_dir,
               if (!is.null(override_dir)) paste0(" or override '", override_dir, "'") else ""))
}

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
  # col1 = time (s), col2 = name
  names_raw <- trimws(mk$X2)
  
  # make names unique with suffix counting like Python logic
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
    time = mk$X1 * 1000 # s -> ms
  )
}
#---------
# read json and rebuild signal
read_data_json <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  
  samples <- j$Samples
  # If the key doesn't exist OR it's empty -> return NULL
  if (is.null(samples) || length(samples) == 0) {
    return(NULL)
  }
  
  # keep only those with MeasIMU9
  meas <- purrr::keep(samples, ~ !is.null(.x$MeasIMU9))
  if (length(meas) == 0) return(NULL)
  
  meas <- purrr::map(meas, "MeasIMU9")
  
  out <- purrr::map(meas, function(m) {
    if (is.null(m$Timestamp) || is.null(m$ArrayAcc)) return(NULL)
    list(Timestamp = m$Timestamp, ArrayAcc = m$ArrayAcc)
  }) |> purrr::compact()
  
  if (length(out) == 0) return(NULL)
  
  out
}

filt_norm_signal <- function(x, fs) {
  nyq <- 0.5 * fs
  
  # high-pass 0.5 Hz, order 4
  hp <- signal::butter(4, 0.5 / nyq, type = "high")
  x  <- signal::filtfilt(hp, x)
  
  # low-pass 10 Hz, order 4
  lp <- signal::butter(4, 10 / nyq, type = "low")
  x  <- signal::filtfilt(lp, x)
  
  x
}

create_signal <- function(data, fs = 208, n_samples = 8) {
  period <- 1000 / fs # ms
  x <- numeric(0); y <- numeric(0); z <- numeric(0)
  timepoints <- numeric(0)
  
  for (dp in data) {
    ts0 <- dp$Timestamp
    samples <- dp$ArrayAcc
    
    n <- length(samples)
    if (n != n_samples) {
      message(n, " samples (expected ", n_samples, ") in packet at ts=", ts0, " (no interpolation implemented)")
    }
    
    for (s in samples) {
      x <- c(x, s$x)
      y <- c(y, s$y)
      z <- c(z, s$z)
    }
    timepoints <- c(timepoints, ts0 + (0:(n - 1)) * period)
  }
  
  intensity_unfiltered <- sqrt(x^2 + y^2 + z^2)
  
  fx <- filt_norm_signal(x, fs)
  fy <- filt_norm_signal(y, fs)
  fz <- filt_norm_signal(z, fs)
  
  intensity_filtered <- sqrt(fx^2 + fy^2 + fz^2)
  
  list(
    intensities = intensity_filtered,
    unfiltered  = intensity_unfiltered,
    timestamps  = timepoints
  )
}

local2utc <- function(timestamp_ms, local_ms, utc_us) {
  timestamp_ms - local_ms + (utc_us / 1000)
}

#-------------------
# load one session and override markers if provided
load_acceleration_session <- function(raw_root, subject_rel_path,
                                      corrected_timemap_root = NULL,
                                      fs = 208, n_samples = 8) {
  raw_dir <- file.path(raw_root, subject_rel_path)
  
  # corrected timemap leaf folder (same relative path) – optional
  corr_tm_dir <- if (!is.null(corrected_timemap_root)) {
    file.path(corrected_timemap_root, subject_rel_path)
  } else NULL
  
  # JSON + marker always from raw
  json_file   <- pick_one2(raw_dir, "\\.json$")
  marker_file <- pick_one2(raw_dir, "marker.*\\.txt$")
  
  # timemap: prefer corrected folder, else raw
  timemap_file <- pick_one2(raw_dir, "timemap.*\\.txt$", override_dir = corr_tm_dir)
  
  data <- read_data_json(json_file)
  if (is.null(data)) {
    stop("Empty/invalid JSON samples in: ", json_file)
  }
  tm     <- read_timemap(timemap_file)
  events <- read_markers(marker_file)
  
  sig <- create_signal(data, fs = fs, n_samples = n_samples)
  time_utc <- local2utc(sig$timestamps, local_ms = tm$local, utc_us = tm$utc)
  
  df <- tibble::tibble(
    time_utc_ms = time_utc,
    timestamp_ms = sig$timestamps,
    intensity = sig$intensities,
    intensity_unfiltered = sig$unfiltered,
    subject_path = subject_rel_path
  )
  
  list(
    data = df,
    events = events,
    timemap = tm,
    files = list(json = json_file, timemap = timemap_file, marker = marker_file)
  )
}


#example
# res <- load_acceleration_session(
#   raw_root = "data",
#   subject_rel_path = file.path("April 1", "Session 1", "1990"),
#   corrected_timemap_root = file.path("data", "_Corrected Timestamp Files")
# )
# 
# res$data
# res$events
# res$files

# find all session folders
find_session_leaf_folders <- function(raw_root) {
  # all folders under raw_root (including raw_root)
  dirs <- c(raw_root, list.dirs(raw_root, recursive = TRUE, full.names = TRUE))
  
  keep <- vapply(dirs, function(d) {
    if (!dir.exists(d)) return(FALSE)
    
    files <- list.files(d, full.names = FALSE, ignore.case = TRUE)
    
    has_json   <- any(grepl("\\.json$", files, ignore.case = TRUE))
    has_marker <- any(grepl("marker.*\\.txt$", files, ignore.case = TRUE))
    has_tm     <- any(grepl("timemap.*\\.txt$", files, ignore.case = TRUE))
    
    has_json && has_marker && has_tm
  }, logical(1))
  
  dirs[keep]
}
# batch load data
load_all_sessions <- function(raw_root = "data",
                              corrected_timemap_root = file.path("data", "_Corrected Timestamp Files"),
                              fs = 208, n_samples = 8) {
  
  leaf_dirs <- find_session_leaf_folders(raw_root)
  raw_root_norm <- normalizePath(raw_root, winslash = "/", mustWork = TRUE)
  
  rel_paths <- vapply(leaf_dirs, function(d) {
    d_norm <- normalizePath(d, winslash = "/", mustWork = TRUE)
    sub(paste0("^", raw_root_norm, "/?"), "", d_norm)
  }, character(1))
  
  all_data <- list()
  sessions <- list()
  
  errors <- tibble::tibble(
    subject_path = character(),
    error = character()
  )
  
  for (i in seq_along(rel_paths)) {
    p <- rel_paths[i]
    message(sprintf("[%d/%d] Loading: %s", i, length(rel_paths), p))
    
    out <- tryCatch(
      load_acceleration_session(
        raw_root = raw_root,
        subject_rel_path = p,
        corrected_timemap_root = corrected_timemap_root,
        fs = fs, n_samples = n_samples
      ),
      error = function(e) e
    )
    
    if (inherits(out, "error")) {
      errors <- dplyr::add_row(errors, subject_path = p, error = out$message)
      next
    }
    
    sessions[[p]] <- out
    all_data[[p]] <- out$data
  }
  
  list(
    all_data = dplyr::bind_rows(all_data),
    sessions = sessions,
    errors = errors
  )
}


#run it on project
all <- load_all_sessions(raw_root = "data",
                         corrected_timemap_root = file.path("data", "_Corrected Timestamp Files"))

all$all_data
all$errors



