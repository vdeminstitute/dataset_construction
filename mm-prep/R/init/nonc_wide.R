#!/usr/bin/env Rscript

# ==========================================================================
# 
# ==========================================================================
options(warn = 2)
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "nonc_wide")


#
# Functions
# --------------------------------------------------------------------------
delete_column <- function(df, qtable, VARNAME) {
	stopifnot(grepl("^v[2|3]", VARNAME))
	df %<>% left_join(select(qtable, code_col, text_col, question_id),
                  by = "question_id")
	if (isTRUE(unique(df$code_col))) {
	    df[[VARNAME]] <- df[["code"]]
	} else {
	    df[[VARNAME]] <- df[["text_answer"]]
	}
	df$code <- NULL
	df$text_answer <- NULL

	return(df)
}

subset_cols <- function(df, VARNAME) {
	df %>% select(country_id, historical_date, year,
               one_of(VARNAME)) %>%
		return(.)
}

main <- function(df, qtable, VARNAME) {
	df <- delete_column(df, qtable, VARNAME)
	df <- subset_cols(df, VARNAME)
	with(df, stopifnot(!is.na(country_id), !is.na(historical_date), 
		!is.na(year)))
	return(df)
}

#
# Write output
# --------------------------------------------------------------------------

# Run script
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    qtable <- load_qtable()
    objs <- find_dep_files(TASK_ID, DB)
    df <- objs[[VARNAME]][[VARNAME]]

    # Run
    collectedInputs <- named_list(df, qtable, VARNAME)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with nonc_wide...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/init/test_nonc_wide.R")
}
update_task_status(db = DB)