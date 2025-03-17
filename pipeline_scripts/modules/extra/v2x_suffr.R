#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# The This script creates the v2x_suffr index, by taking v2elsuffrage / 100. 
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
main <- function(df) {
	stopifnot(is.data.frame(df),
		c("v2elsuffrage") %in% names(df),
		max(df[["v2elsuffrage"]]) <= 100,
		min(df[["v2elsuffrage"]]) >= 0)
	df <- mutate(df, v2x_suffr = v2elsuffrage / 100) %>%
 	 	select(-v2elsuffrage)

    return(df)
}

# Run script
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    objs <- find_dep_files(TASK_ID, db)
	df <- objs[["v2elsuffrage"]][["v2elsuffrage"]]

    # Run
    collectedInputs <- named_list(df)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with script...")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/extra/test_v2x_suffr.R")
}
