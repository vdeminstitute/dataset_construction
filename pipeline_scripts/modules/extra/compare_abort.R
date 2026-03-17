#!/usr/bin/env Rscript

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))
suppressMessages(library(DBI))

# ========================================================================== #
# In the script, we compare the database version of hosabort and legabort to what is calculated.
# The output of this is a product for A Data Team.
#
# --------------------------------------------------------------------------
# Global variables
db <- pg_connect()
get_globals()

# Imports
qtable <- load_qtable()
objs <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]$cd

info(sprintf("Comparing calculated %s to database version", TASK_NAME))
vdem_data <- pg_connect(Sys.getenv("DOWNLOAD_DB"))
prev_version <- dbGetQuery(vdem_data,
    sprintf("SELECT * FROM a_data WHERE question_id IN (SELECT question_id FROM question WHERE name = '%s');", TASK_NAME)) %>%
    select(country_id, historical_date, code) 

compare_df <- full_join(objs, prev_version, by = c("country_id", "historical_date")) %>%
    mutate(diff = case_when(
        is.na(code) & is.na(.[[TASK_NAME]]) ~ FALSE,
        is.na(code) & !is.na(.[[TASK_NAME]]) ~ TRUE,
        !is.na(code) & is.na(.[[TASK_NAME]]) ~ TRUE,
        TRUE ~ code != .[[TASK_NAME]])) %>%
    rename(!!paste0(TASK_NAME, "_calc", sep = "") := all_of(TASK_NAME),
        !!TASK_NAME := code) %>%
    arrange(country_id, historical_date)

# write file
write_file(compare_df, gsub("[.]rds$", ".csv", OUTFILE), dir_create = TRUE)