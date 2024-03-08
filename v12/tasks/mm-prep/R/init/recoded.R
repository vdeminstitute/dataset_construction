#!/usr/bin/env Rscript

# ==========================================================================
options(warn = 2)
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "recoded")

#
# Functions
# --------------------------------------------------------------------------
append_info <- function(df, qtable) {
	df %>%
		left_join(select(qtable, recoded, question_id, name), by = "question_id") %>%
		return(.)
}

create_recoded <- function(df, qtable) {
	df  %<>%
    filter(recoded) %>%
    mutate(code = case_when(
        code == 0 ~ 0,
        code %in% c(1, 2) ~ 1,
        code %in% c(3, 4) ~ 2,
        TRUE ~ NA_real_
        )) %>%
    mutate(name = name %^% "_rec") %>%
    mutate(question_id =
        trans(name, to = "question_id", ttable = qtable, by = "name"))
    stopifnot(!anyNA(df$code))
    return(df)
}

main <- function(df, qtable) {
	append_info(df, qtable) %>%
		create_recoded(qtable) %>%
		return(.)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
    get_globals()

    # Imports
    qtable <- load_qtable()
    df <- find_dep_files(TASK_ID, DB)[[gsub("_rec", "", VARNAME, fixed = TRUE)]][[gsub("_rec", "", VARNAME, fixed = TRUE)]]

    # Run
    collectedInputs <- named_list(df, qtable)
    do.call(main, collectedInputs) %>%
    	list() %>%
    	setNames(VARNAME) %>%
    	write_file(OUTFILE, dir_create = TRUE)
    info("Create recoded variable versions for " %^% VARNAME)
} else {
# Call unit tests for main function and sub functions
    testthat::test_file("~/proj/mm-prep/tests/init/test_recoded.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)