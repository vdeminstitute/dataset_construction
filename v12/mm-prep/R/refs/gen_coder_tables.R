#!/usr/bin/env Rscript
#
#
#
# 
# Generates coder table with:
#   1. Lateral boolean
#   2. Historical boolean
#   3. New coder boolean (coded >= 2005 for old countries)
#   4. Main country, if not provided, fallback to country coded most
#      per survey
#
# coder_table.rds
###

options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "coder_table")

#
# Functions
determine_coder_types <- function(df, country_unit, main_country, lateral_dates) {
    raw_min <-
        country_unit %>%
        group_by(country_id) %>%
        summarise(codingstart = min(year), .groups = "drop")
    new_coder_date <- as.Date("2005-01-01")
    
    df %>%
        left_join(lateral_dates, by = c("country_id", "historical_date")) %>%
        group_by(coder_id, question_id, country_id) %>%
        summarise(historical = all(data_type == "historical"),
                  count = n(),
                  min_date = min(historical_date),
                  lateral = all(!is.na(lateral_id)), 
                  .groups = "drop") %>%
        left_join(raw_min, by = "country_id") %>%
        mutate(new_coder = ifelse(historical | lateral,
                              FALSE,
                              codingstart < new_coder_date & min_date >= new_coder_date)) %>%
        left_join(main_country, by = "coder_id") %>%
        select(-min_date, -codingstart) %>%
        return(.)
}
###
# Calculate who's historical, lateral, and new and append
# main_country_id. Find min historical date for each country and
# convert our crucial dates to Date objects to slightly speed up our
# later ops.


fix_lateral_only <- function(df, df_original, country) {
    ###
    # For some historical countries we only have lateral coders, mark them
    # as normal so the model script can properly set the priors.
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

determine_sequential_coder <- function(df, qtable, VARNAME, coder) {
    backfill_coders <- 
        coder %>%
        mutate(backfill_coder =
            ifelse(is.na(backfill_coder), FALSE, backfill_coder)) %>%
        filter(backfill_coder) %$% coder_id

    df %<>%
        left_join(qtable[, c("question_id", "name")], by = "question_id") %>%
        rename(question_name = name)

    if (qtable$backfill_question) {
        df %<>% mutate(seq_coder = coder_id %in% backfill_coders)
        # Set new coder to FALSE for sequential coders!
        df %<>% mutate(new_coder = ifelse(seq_coder, FALSE, new_coder))
    } else {
        df$seq_coder <- FALSE
    }   
    return(df)
}

upload_copy_to_db <- function(df, db) {
    # remove question from coder_table before uploading
    delete_coder_question(unique(df$question_id), db)
    # add question to coder_table
    df %>% 
        rename(lateral_ = lateral, count_ = count) %>%
        pg_append_table(., "pipe.coder_table", db)
    return(NULL)
}

main <- function(qtable, country, country_unit, df, main_country, lateral_dates, coder, VARNAME, db) {
    country %<>% rename(country_name = name)
    qtable %<>% filter(name == VARNAME)
    country_unit %<>% select(-historical_date)
    main_country %<>% select(coder_id, main_country_id = country_id)
    stopifnot(all(df$coder_id %in% main_country$coder_id))

    df_original <- df
    df %<>% 
        determine_coder_types(country_unit, main_country, lateral_dates) %>%
        fix_lateral_only(df_original, country) %>%
        determine_sequential_coder(qtable, VARNAME, coder)

    upload_copy_to_db(df, db)
    return(df)    
}


# Run script
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    qtable <- load_qtable()
    country <- load_country()
    country_unit <- load_country_unit()
    coder <- read_file(file.path(ROOT, "download", "coder.rds"))
    main_country <- read_file(file.path(ROOT, "refs", "main_country.rds"))
    df <- find_dep_files(TASK_ID, DB)[[VARNAME]][[VARNAME]]
    lateral_dates <- read_file(file.path(ROOT, "download", "lateral_dates.rds"))

    # Run
    collectedInputs <- named_list(qtable, country, country_unit, df, 
        main_country, lateral_dates, coder, VARNAME, db)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = T)    
} else {
    testthat::test_file("~/proj/mm-prep/tests/refs/gen_coder_tables.R")
}
update_task_status(db = DB)