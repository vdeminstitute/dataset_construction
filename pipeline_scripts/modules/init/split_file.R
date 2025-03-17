#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Place the unnested mc and ms files in individual files.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
    db <- pg_connect()
    get_globals()

    # Run
    setNames(list(find_dep_files(TASK_ID, db)[[1]][[TASK_NAME]]), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done splitting file for " %^% TASK_NAME)
} else {
	# Call unit tests for main function and sub functions
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_split_file.R")
}
