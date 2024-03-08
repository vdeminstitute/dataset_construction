#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Script to clean variables after measurement model have been run.
# This scripts is generic and thus needs to know which variable to clean and
# which variable to use for cleaning.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Functions
# --------------------------------------------------------------------------
main <- function(objs, country, utable, cleaning_var, TASK_NAME) {
    
    # Create out-object from objs:varname
    out <- list()
    out[[TASK_NAME]] <- objs[[TASK_NAME]][[TASK_NAME]]
    # Remove current cd/cy objects
    out[[TASK_NAME]]$cd <- NULL
    out[[TASK_NAME]]$cy <- NULL
    
    # Loop over cd/cy to create new versions
    lapply(c("cd", "cy"), function(lev) {
        date_col <- if(lev == "cd") "historical_date" else "year"
        drop_col <- if(date_col == "historical_date") "year" else "historical_date"

        df <- add_country_cols(objs[[TASK_NAME]][[TASK_NAME]][[lev]], country)
        n <- names(df)
        df_clean <- add_country_cols(objs[[cleaning_var]][[cleaning_var]][[lev]], country)
        df_clean[[drop_col]] <- NULL
        df[[drop_col]] <- NULL

        # Adjust name of cleaning_var here if needed. Example:
        # -- 2elfferlrbin_ord, because v2elffelr is mm and we want to clean by discrete values.
        if (TASK_NAME == "v2elffelr") {
            cleaning_var <- "v2elffelrbin_ord"
        }

        # Create data.frame to clean 
        full_df <-
            full_join(df, df_clean,
                by = c("country_id", "country_text_id", date_col)) %>%
            add_date_cols() %>%
			interpolate_components(., cleaning_var, utable, 
				coder_level = FALSE)
        # bool is hard coded to NA | 0L, not dynamically set
		bool <- is.na(full_df[[cleaning_var]]) | full_df[[cleaning_var]] == 0L
        stopifnot(!any(is.na(bool)))
		full_df <- clean_observations(
            df = full_df,
            bool = bool,
            function_name = "main",
            description = "Post mm cleaning if cleaning variable is missing or 0")
        
        # Clean up
        full_df %<>% select(any_of(n), -country_text_id)
        full_df <- full_df[!is.na(full_df[[TASK_NAME]]), ] # keeping only non-NA
    
    # Insert cleaned df into out
    out[[TASK_NAME]][[lev]] <<- full_df

    }) %>% invisible()
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
    utable <- load_country_unit()
    objs <- find_dep_files(TASK_ID, db)
    cleaning_var <- names(objs)[names(objs) != TASK_NAME]

    # Run
    collectedInput <- named_list(objs, country, utable, cleaning_var, TASK_NAME)
    do.call(main, collectedInput) %>%
        write_file(., OUTFILE, dir_create = TRUE)

    } else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_post_mm_cleaning.R")
    }
