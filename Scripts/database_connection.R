library(duckdb)
library(DBI)
library(dplyr)
library(dbplyr)

# connect to DuckDB
con <- dbConnect(
  duckdb::duckdb(),
  dbdir = "my_database.duckdb"
)
#checking connection
dbListTables(con)


# 3) Write into DuckDB as a table
dbWriteTable(con, "acc_data", acc_all_data, overwrite = TRUE)

# 4) Use dbplyr
acc_data <- tbl(con, "acc_data")


## Load Metadata in
meta <- read_csv("data/valid_sessions.csv")

dbWriteTable(
  con,
  name = "metadata",
  value = meta,
  overwrite = TRUE
)

meta_tbl <- tbl(con, "metadata")


#create key for joining
acc_data_clean <- acc_data %>%
  mutate(
    date_folder = sql("regexp_extract(subject_path, '^([^/]+)', 1)"),
    session_nr  = sql("CAST(regexp_extract(subject_path, 'Session ([0-9]+)', 1) AS INTEGER)"),
    sensor_chr  = sql("CAST(regexp_extract(subject_path, '([^/]+)$', 1) AS INTEGER)"),
    key     = sql("concat(date_folder, '|', session_nr, '|', sensor_chr)")
  ) %>%
  compute(name = "acc_data_clean", temporary = FALSE)
