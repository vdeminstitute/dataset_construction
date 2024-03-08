#!/usr/bin/env Rscript

# ==========================================================================
# Remove observations for indices
# ==========================================================================

options(warn = 2)

suppressMessages(library(vutils))
suppressMessages(library(dplyr))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
prep_df <- function(df_ll, country, lev) {
    df_ll[[lev]] %>%
        add_country_cols(country) %>% 
        add_date_cols
}

prep_df_coders <- function(df_coders, ncomps, TASK_NAME) {
    if (nrow(df_coders) == 0) {
        return(data.frame())
        } else {
           bool <- apply(select(df_coders, -country_id, -year), 1, function(x) {
            (sum(x, na.rm = TRUE) / ncomps) >= 0.33
        })
            df_coders %<>% select(country_id, year) %>% filter(bool)
            info("Removing these observations for " %^% TASK_NAME %^% ": " %^%
                df_coders)
            return(df_coders) 
        }
}

final_prep <- function(df, df_coders) {
    if (nrow(df_coders) == 0) {
        return(df)
    } else {
        df <- anti_join(df, df_coders, by = c("country_id", "year"))
        return(df)
    }
}

main <- function(df_ll, country, df_coders, ncomps, TASK_NAME) {
    out <- lapply(c("cd", "cy"), function(lv) {
        df <- prep_df(df_ll, country, lv)
        df_coders <- prep_df_coders(df_coders, ncomps, TASK_NAME)
        df <- final_prep(df, df_coders)
        })
    names(out) <- c("cd", "cy")
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

    ncomps <- length(names(objs)) - 1
    df_ll <- objs[[TASK_NAME]][[TASK_NAME]]
    df_coders <- lapply(names(objs)[!names(objs) %in% TASK_NAME], function(v) {
        rr <- objs[[v]][[v]][["attrition_cleaning"]]
        if (is.null(rr))
            rr <- data.frame()
        rr
        }) %>% Reduce(full_join_vdem, .)

    # Run
    collectedInuts <- named_list(df_ll, country, df_coders, ncomps, TASK_NAME)
    do.call(main, collectedInuts) %>%
        list() %>%
        setNames(TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)

    } else {
        testthat::test_file("~/proj/vdemds/module_unit_tests/index/test_attrition_cleaning_index.R") %>%
		check_test_file_output()
    }
