#!/usr/bin/env Rscript

# Purpose: We want to generate a data.frame that indicates for each combination
# of variable, coder, and country, the 'coder type'. The following types exist:
# 1. Historical_coder: only codes historical periods and variables.
# 2. Lateral coder: only codes a few observations typically around 1900 and 2012
#   or on election-dates around the same years.
# 3. New coder: only codes from 2005 and onwards.
# 4. Sequential coder: codes from 2005 and onwards but also with observations from
#   1900 and then in steps of 20-25 years.
# Note that in this circumstance we do not care about the other contemporary coders.
# 
# The output includes the main country coded by the coder. Do note that the
# concept of 'main_country' is constant across a coder.
#
# The input that is used to determine the coder types is the following:
# - country_unit table: produced every version
# - coder table: hardcoded from Postgres
# - main_country table: produced every version
# - lateral_dates table: hardcoded from Postgres
# Hence, coder types are induced from the data.
# 
# This script gets run once per variable, and this produce one coder_table 
# per variable. However, in postgres we only have one large coder_table.

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# ------------------------------------------------------------------------------
# Functions

# Determine_coder_types
# historical is TRUE if all coder scores for a given coder, question, and country
# 	are in historical time periods as defined by the country_unit table.
# lateral is TRUE if all coder scores for a given coder, question, and country
# 	are on lateral dates as defined by the lateral_dates table.
# new_coder is TRUE if all observations given a coder, question, and country
# 	are in the newer time period (start from 2005).
determine_coder_types <- function(df, country_unit, main_country, lateral_dates) {
    
    raw_min <-
        country_unit %>%
        group_by(country_id) %>%
        summarise(codingstart = min(year), .groups = "drop")
    new_coder_date <- as.Date("2005-01-01")
    
    df <-
        left_join(df, lateral_dates, by = c("country_id", "historical_date"),
            relationship = "many-to-many") %>%
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

# Alter edge cases from historical countries
# For some historical countries we only have lateral coders. This causes the 
# model script to fail with setting priors. The solution is to mark these coders
# "normal" coders.
fix_lateral_only <- function(df, df_original, country) {
    
    df %<>%
        group_by(country_id, question_id) %>%
        mutate(lateral = if (all(lateral)) FALSE else lateral) %>% 
        left_join(country[, c("country_id", "country_text_id")],
            by = "country_id") %>%
        ungroup()
    # If we're missing something by this point, we're screwed.
    stopifnot(!any(is.na(df$main_country_id)))
    stopifnot(length(setdiff(df$coder_id, df_original$coder_id)) == 0)

    return(df)
}

# Determine sequential coders
# A given coder is flagged as a sequential if: 
# (1) The column backfill_coder from coder table is _not_ missing
# and
# (2) The column backfill_question from qtable is set to TRUE (see qtable)
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
        # Set new coder to FALSE for sequential coders!
        df %<>% mutate(new_coder = ifelse(seq_coder, FALSE, new_coder))
    } else {
        df$seq_coder <- FALSE
    }
    return(df)
}

# Append the resulting data.frame to the database table coder_table
# We need to rename certain fields because Postgres has these as reserved names.
upload_copy_to_db <- function(df, db) {

    # Remove question from coder_table before uploading
    vutils::delete_coder_question(unique(df$question_id), db)
    # Add question to coder_table
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
    lateral_dates <- read_file(file.path(ROOT, "download", "lateral_dates.rds"))
	lateral_dates %<>% select(lateral_id, country_id, historical_date)
	
    # Run
    collectedInputs <- named_list(qtable, country, country_unit, df, 
        main_country, lateral_dates, coder, TASK_NAME, db)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/refs/gen_coder_tables.R")
}
