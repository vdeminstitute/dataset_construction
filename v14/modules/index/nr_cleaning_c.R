#!/usr/bin/env Rscript
# This script is used to clean cd and cy summarizes of mm variables based on the number of coders per country observation. This script is applied to variables based on demand (i.e. depending on orders from the project managers). At the moment the rule for cleaning an observation is less than 3 coders.
# --------------------------------------------------------------------------
suppressMessages(library(vutils))
suppressMessages(library(dplyr))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
main <- function(objs, country, TASK_NAME) {

    out <- list()
    invisible(lapply(c("cd", "cy"), function(lev) {

        v_nr <- paste0(TASK_NAME, "_ncoders")
        v <- names(objs[!names(objs) %in% v_nr])
        df <- objs[[v]][[TASK_NAME]][[lev]] %>%
            add_country_cols(country) %>%
            add_date_cols()
        df_ncoders <-
            objs[[v_nr]][[TASK_NAME]][[lev]] %>%
            add_country_cols(country)

        df <- full_join_vdem(df, df_ncoders)

        # If the number of coders per country year / date observation is less than 3 we drop it
        bool <- df[[paste0(TASK_NAME, "_nr")]] < 3
        bool[is.na(bool)] <- FALSE
        out[[TASK_NAME]][[lev]] <<- clean_observations(df, bool, "nr_cleaning_c", "country observations dropped due to too few coders") |> select(-matches("_nr$"))
        
    }))
    return(out)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    country <- load_country()
    objs <- find_dep_files(TASK_ID, db)

    # Run
    collectedInputs <- named_list(objs, country, TASK_NAME)
    do.call(main, collectedInputs) %>%
        write_file(., OUTFILE, dir_create = TRUE)

    } else {
        testthat::test_file("~/proj/vdemds/module_unit_tests/index/test_nr_cleaning_c.R")
    }
