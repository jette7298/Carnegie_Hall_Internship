library(tidyverse)

# ------------------------------------------------------------------
# 1) Load metadata: which sessions are valid
# ------------------------------------------------------------------
valid <- read_csv("data/valid_sessions.csv")

raw_root <- "data"

# ------------------------------------------------------------------
# 2) Find all "leaf" folders that contain one complete recording
#    (json + marker + timemap)
# ------------------------------------------------------------------
find_session_leaf_folders <- function(root) {
  dirs <- list.dirs(root, recursive = TRUE, full.names = TRUE)
  
  dirs[vapply(dirs, function(d) {
    files <- list.files(d, ignore.case = TRUE)
    any(grepl("\\.json$", files)) &&
      any(grepl("marker.*\\.txt$", files)) &&
      any(grepl("timemap.*\\.txt$", files))
  }, logical(1))]
}

leaf_dirs <- find_session_leaf_folders(raw_root)

# ------------------------------------------------------------------
# 3) Convert absolute paths → relative paths
#    e.g. "data/April 1/Session 1/1990" → "April 1/Session 1/1990"
# ------------------------------------------------------------------
root_norm <- normalizePath(raw_root, winslash = "/")

rel_paths <- sub(
  paste0("^", root_norm, "/?"),
  "",
  normalizePath(leaf_dirs, winslash = "/")
)

# ------------------------------------------------------------------
# 4) Build keys from folder structure
#    "April 1/Session 1/1990" → "April 1|1|1990"
# ------------------------------------------------------------------
make_leaf_key <- function(path) {
  parts <- strsplit(path, "/")[[1]]
  paste(
    parts[1],                              # date folder
    str_extract(parts[2], "\\d+"),         # session number
    parts[length(parts)],                  # sensor ID
    sep = "|"
  )
}

leaf_key_df <- tibble(
  subject_rel_path = rel_paths,
  key = map_chr(rel_paths, make_leaf_key)
)

# ------------------------------------------------------------------
# 5) Keep only folders that appear in valid_sessions.csv
# ------------------------------------------------------------------
to_load <- leaf_key_df$subject_rel_path[
  leaf_key_df$key %in% valid$valid_key
]

length(to_load)   # ~180 (expected)


# ------------------------------------------------------------------
# save list of valid session folders
# ------------------------------------------------------------------
saveRDS(to_load, file = "data/to_load_subject_paths.rds")
