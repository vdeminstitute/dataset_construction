#!/usr/bin/env Rscript
options(warn = 2)

suppressMessages(library(vutils))
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "nr_cleaning_index")

#
# Functions
# --------------------------------------------------------------------------
main <- function(objs, country, VARNAME) {
    out <- list()
    lapply(c("cd", "cy"), function(lev) {
        df_coders <- lapply(names(objs)[!names(objs) %in% VARNAME], function(v) {
            objs[[v]][[v]][[lev]] %>%
            add_country_cols(country)
            }) %>% Reduce(full_join_vdem, .)
        df <- objs[[VARNAME]][[VARNAME]][[lev]] %>%
            add_country_cols(country) %>% add_date_cols
        df <- full_join_vdem(df, df_coders)
        
        coder_df <- df[, names(objs)[!names(objs) %in% VARNAME] %^% "_nr"]
        bool <- apply(coder_df, 1, function(x) {sum(x >= 3, na.rm = TRUE)}) < 3
        out[[VARNAME]][[lev]] <<-
            df %>% filter(!bool) %>% select(-matches("_nr$"))
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
    collectedInput <- named_list(objs, country, VARNAME)
    do.call(main, collectedInput) %>%
        write_file(., OUTFILE, dir_create = TRUE)

    } else {
        testthat::test_file("~/proj/mm-prep/tests/index/test_nr_cleaning_index.R")
    }
update_task_status(db = DB)
