#!/usr/bin/env Rscript
options(warn = 2)

suppressMessages(library(vutils))
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "nr_cleaning_c")

#
# Functions
# --------------------------------------------------------------------------
main <- function(objs, country, VARNAME) {
    out <- list()
    lapply(c("cd", "cy"), function(lev) {

        v_nr <- VARNAME %^% "_ncoders"
        v <- names(objs[!names(objs) %in% v_nr])
        df <- objs[[v]][[VARNAME]][[lev]] %>%
            add_country_cols(country) %>%
            add_date_cols
        df_ncoders <-
            objs[[v_nr]][[VARNAME]][[lev]] %>%
            add_country_cols(country)

        df <- full_join_vdem(df, df_ncoders)

        bool <- df[[VARNAME %^% "_nr"]] < 3
        bool[is.na(bool)] <- FALSE
        out[[VARNAME]][[lev]] <<- df %>% filter(!bool) %>% select(-matches("_nr$"))
    }) %>% invisible
    return(out)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    country <- load_country()
    objs <- find_dep_files(TASK_ID, DB)

    # Run
    collectedInputs <- named_list(objs, country, VARNAME)
    do.call(main, collectedInputs) %>%
        write_file(., OUTFILE, dir_create = TRUE)

    } else {
        testthat::test_file("~/proj/mm-prep/tests/index/test_nr_cleaning_c.R")
    }
update_task_status(db = DB)
