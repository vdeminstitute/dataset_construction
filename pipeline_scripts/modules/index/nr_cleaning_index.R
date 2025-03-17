#!/usr/bin/env Rscript

# --------------------------------------------------------------------------
# This script is used to clean cd and cy summarizes of indexes based on the number of components per observation and the number of coders per component. At the moment the rule for cleaning is that there should be more than 3 observations per component and 3 or more coders per component.
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
        
        df_coders <- lapply(names(objs)[!names(objs) %in% TASK_NAME], function(v) {
            objs[[v]][[v]][[lev]] %>%
            add_country_cols(country)
            }) %>% Reduce(full_join_vdem, .)
        df <- objs[[TASK_NAME]][[TASK_NAME]][[lev]] %>%
            add_country_cols(country) %>% add_date_cols
        df <- full_join_vdem(df, df_coders)
        
        coder_df <- df[, paste0(names(objs)[!names(objs) %in% TASK_NAME], "_nr")]
        # Drop index observations if there are less than 3 components, where there are 2 or less coders per component
        bool <- apply(coder_df, 1, function(x) {sum(x >= 3, na.rm = TRUE)}) < 3
        out[[TASK_NAME]][[lev]] <<- clean_observations(df, bool, "nr_cleaning_index", "observations dropped due to not enough components and/or coders") |> select(-matches("_nr$"))

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
    collectedInput <- named_list(objs, country, TASK_NAME)
    do.call(main, collectedInput) %>%
        write_file(., OUTFILE, dir_create = TRUE)

    } else {
        testthat::test_file("~/proj/vdemds/module_unit_tests/index/test_nr_cleaning_index.R")
    }
