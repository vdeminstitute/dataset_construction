#!/usr/bin/env Rscript

# ==========================================================================
# This script applies to only to non C data. It removes unwanted columns.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
delete_column <- function(df, qtable, TASK_NAME) {
	stopifnot(grepl("^v[2|3]", TASK_NAME))
	df <- left_join(df, select(qtable, code_col, text_col, question_id),
                  by = "question_id")
	if (isTRUE(unique(df$code_col))) {
	    df[[TASK_NAME]] <- df[["code"]]
	} 
	if (isTRUE(unique(df$text_col))) {
	    df[[TASK_NAME]] <- df[["text_answer"]]
	}
	df$code <- NULL
	df$text_answer <- NULL

	return(df)
}

subset_cols <- function(df, TASK_NAME) {
	df <- select(df, country_id, historical_date, year,
		one_of(TASK_NAME))

	return(df)
}

main <- function(df, qtable, TASK_NAME) {
	df <- delete_column(df, qtable, TASK_NAME)
	df <- subset_cols(df, TASK_NAME)
	with(df, stopifnot(!is.na(country_id), !is.na(historical_date), 
		!is.na(year)))
	return(df)
}

# Run script
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
	db <- pg_connect()
    get_globals()

    # Imports
    qtable <- load_qtable()
    objs <- find_dep_files(TASK_ID, db)
    df <- objs[[TASK_NAME]][[TASK_NAME]]

    # Run
    collectedInputs <- named_list(df, qtable, TASK_NAME)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with nonc_wide...")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_nonc_wide.R") %>%
		check_test_file_output()
}
