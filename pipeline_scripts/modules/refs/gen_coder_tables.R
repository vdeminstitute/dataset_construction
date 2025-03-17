#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Purpose: We want to generate a data.frame that indicates for each combination
# of variable, coder, and country, the 'coder type'. The following types exist:
# 1. Historical_coder
# 2. Lateral coder
# 3. New coder
# 4. Sequential coder

# The output includes the main country coded by the coder.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Functions
# ------------------------------------------------------------------------------
# Determine_coder_types
determine_coder_types <- function(df, country_unit, main_country, lateral_dates) {
    
    raw_min <-
        country_unit %>%
        group_by(country_id) %>%
        summarise(codingstart = min(year), .groups = "drop")
    new_coder_date <- as.Date("2005-01-01")

    df %<>%
        left_join(lateral_dates, by = c("country_id", "historical_date")) %>%
        group_by(coder_id, question_id, country_id) %>%
        summarise(historical = all(data_type == "historical"),
            count = n(),
            min_date = min(historical_date),
            lateral = all(!is.na(lateral_id)), 
            .groups = "drop") %>%
        left_join(raw_min, by = "country_id") %>%
        mutate(
            new_coder = ifelse(historical | lateral,
                yes = FALSE,
                no = codingstart < new_coder_date & min_date >= new_coder_date)) %>%
        left_join(main_country, by = "coder_id") %>%
        select(-min_date, -codingstart) 

    return(df)
}

fix_lateral_only <- function(df, df_original, country) {
    
    df %<>%
        group_by(country_id, question_id) %>%
        mutate(lateral = if (all(lateral)) FALSE else lateral) %>% 
        left_join(country[, c("country_id", "country_text_id")],
            by = "country_id") %>%
        ungroup()
    stopifnot(!any(is.na(df$main_country_id)))
    stopifnot(length(setdiff(df$coder_id, df_original$coder_id)) == 0)

    return(df)
}

# Determine sequential coders
determine_sequential_coder <- function(df, qtable, TASK_NAME, coder) {
    
    backfill_coders <- 
        coder %>%
        mutate(
            backfill_coder = ifelse(is.na(backfill_coder),
                yes = FALSE,
                no = backfill_coder)) %>%
        filter(backfill_coder) %$% coder_id
    sprintf("Found %d backfill_coders", length(backfill_coders)) %>% info()
    
    df %<>%
        left_join(qtable[, c("question_id", "name")],
            by = "question_id") %>%
        rename(question_name = name)

    if (isTRUE(qtable$backfill_question)) {
        df %<>% mutate(seq_coder = coder_id %in% backfill_coders)
        df %<>% mutate(new_coder = ifelse(seq_coder, FALSE, new_coder))
    } else {
        df$seq_coder <- FALSE
    }
    return(df)
}

# Append the resulting data.frame to the database table coder_table
upload_copy_to_db <- function(df, db) {

    vutils::delete_coder_question(unique(df$question_id), db)
    df %>%
        rename(lateral_ = lateral, count_ = count) %>%
        pg_append_table(., "coder_table", db)

    return(NULL)
}

# Main function: per variable, return a data.frame with info on coder types.
main <- function(qtable, country, country_unit, df, main_country, lateral_dates,
    coder, TASK_NAME, db) {

    stopifnot(`Some coders are missing a main country` =
        all(df$coder_id %in% main_country$coder_id))

    df_original <- df
    df %<>% 
        determine_coder_types(df = ., country_unit, main_country, lateral_dates) %>%
        fix_lateral_only(df = ., df_original, country) %>%
        determine_sequential_coder(df = ., qtable, TASK_NAME, coder) %>% 
        as.data.frame()

    if (is.grouped_df(df)) {
        df <- ungroup(df)
    }

    upload_copy_to_db(df, db)

    return(df)    
}


# Run script
# ------------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    df <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]
    # Reference tables
    qtable <- load_qtable() %>% filter(name == TASK_NAME)
    country <- load_country() %>% rename(country_name = name)
    country_unit <- load_country_unit() %>% select(-historical_date)
    coder <- read_file(file.path(ROOT, "download", "coder.rds"))
    main_country <- read_file(file.path(ROOT, "refs", "main_country.rds")) %>% 
        select(coder_id, main_country_id = country_id)
    lateral_dates <- read_file(file.path(ROOT, "download", "lateral_dates.rds")) %>% 
        select(lateral_id, country_id, historical_date) %>%
        distinct(country_id, historical_date, .keep_all = TRUE)
	
    # Run
    collectedInputs <- named_list(qtable, country, country_unit, df, 
        main_country, lateral_dates, coder, TASK_NAME, db)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/refs/gen_coder_tables.R")
}
