#!/usr/bin/env Rscript

options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
suppressMessages(library(tidyr))
set_env(MODULE_NAME = "split_mcms")

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
    get_globals()

    # Imports
    df <- find_dep_files(TASK_ID, DB)[[1]][[VARNAME]]

    # Run
    setNames(list(df), VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done splitting file for " %^% VARNAME)
} else {
	# Call unit tests for main function and sub functions
    testthat::test_file("~/proj/mm-prep/tests/init/test_split_file.R")
}
update_task_status(db = DB)