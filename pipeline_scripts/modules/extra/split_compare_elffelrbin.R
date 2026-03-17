#!/usr/bin/env Rscript

# ========================================================================== #
# This script is used to compare the database version (a_data table) of v2elffelrbin with the newly calculated version.
# This script also splits v2elffelrbin and v3elfflrbin, as v2elffelrbin is an A,C variable and v3elffelrbin remains a standard C variable.
# If this changes in the future, the script should be updated accordingly.

# The compare file generated from this script is for the A-Data team to review and validate v2elffelrbin.
# --------------------------------------------------------------------------
suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))
suppressMessages(library(DBI))

# --------------------------------------------------------------------------
# Functions
split_eras <- function(df, TASK_NAME, version) {
    if (TASK_NAME == "v2elffelrbin") {
        if (version == "cd") {
            df %<>% filter(historical_date > "1899-12-31")
            stopifnot(min(df$historical_date) == as.Date("1900-01-01"))
        } else if (version == "cy") {
            df %<>% filter(year >= 1900)
            stopifnot(min(df$year) == 1900)
        }
    } else if (TASK_NAME == "v3elffelrbin") {
        if (version == "cd") {
            df %<>% filter(historical_date < "1900-01-01") 
            names(df) <- gsub("v2elffelrbin", "v3elffelrbin", names(df))
            stopifnot(max(df$historical_date) == as.Date("1899-12-31"))
        } else if (version == "cy") {
            df %<>% filter(year < 1900) 
            names(df) <- gsub("v2elffelrbin", "v3elffelrbin", names(df))
            stopifnot(max(df$year) == 1899)
        }
    }

    return(df)
}
# --------------------------------------------------------------------------
# Run
# Global variables
db <- pg_connect()
get_globals()
country <- load_country()

if (TASK_NAME == "v2elffelrbin") {
    objs <- find_dep_files(TASK_ID, db)[[TASK_NAME]]
    objs_cd <- objs[[TASK_NAME]]$cd
    objs_cy <- objs[[TASK_NAME]]$cy

    df_cd <- split_eras(objs_cd, TASK_NAME, "cd")
    df_cy <- split_eras(objs_cy, TASK_NAME, "cy")

    # compare file
    vdem_data <- pg_connect(Sys.getenv("DOWNLOAD_DB"))
    prev_version <- dbGetQuery(vdem_data,
        sprintf("SELECT * FROM a_data WHERE question_id IN (SELECT question_id FROM question WHERE name = '%s');", TASK_NAME)) %>%
        select(country_id, historical_date, code) 

    compare_df <- df_cd %>%
        select(country_text_id, historical_date, v2elffelrbin_ord) %>%
        left_join(select(country, country_id, country_text_id), by = "country_text_id") %>%
        full_join(prev_version, by = c("country_id", "historical_date")) %>%
        mutate(diff = case_when(
            is.na(code) & is.na(v2elffelrbin_ord) ~ FALSE,
            is.na(code) & !is.na(v2elffelrbin_ord) ~ TRUE,
            !is.na(code) & is.na(v2elffelrbin_ord) ~ TRUE,
            TRUE ~ code != v2elffelrbin_ord)) %>%
        rename(v2elffelrbin_db = code) %>%
        select(country_id, historical_date, v2elffelrbin_ord, v2elffelrbin_db, diff) %>%
        arrange(country_id, historical_date)

    # write compare file
    write_file(compare_df, sprintf("~/data/ds_construction/%s/extra/elffelrbin/%s_compare.csv", Sys.getenv("DEFAULT_DB"), TASK_NAME), dir_create = TRUE)

    out <- list(v2elffelrbin = list(
        cd = df_cd,
        cy = df_cy
    ))

    # write v2 file
    write_file(out, OUTFILE, dir_create = TRUE)

} else if (TASK_NAME == "v3elffelrbin") {
    objs <- find_dep_files(TASK_ID, db)[["v2elffelrbin"]]

    df_cd <- split_eras(objs[["v2elffelrbin"]]$cd, TASK_NAME, "cd")
    df_cy <- split_eras(objs[["v2elffelrbin"]]$cy, TASK_NAME, "cy")

    out <- list(v3elffelrbin = list(
        cd = df_cd,
        cy = df_cy
    ))

    # write v3 file
    write_file(out, OUTFILE, dir_create = TRUE)    
}

