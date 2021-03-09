#!/usr/bin/env Rscript

options(warn = 2)
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "v2x_suffr")

#
# Functions
# --------------------------------------------------------------------------
main <- function(df) {
	stopifnot(is.data.frame(df),
		c("v2elsuffrage") %in% names(df),
		max(df[["v2elsuffrage"]]) <= 100,
		min(df[["v2elsuffrage"]]) >= 0)
	df %>% mutate(v2x_suffr = v2elsuffrage / 100) %>%
 	 	select(-v2elsuffrage) %>%
  		return(.)
}

# Run script
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    objs <- find_dep_files(TASK_ID, DB)
	df <- objs[["v2elsuffrage"]][["v2elsuffrage"]]

    # Run
    collectedInputs <- named_list(df)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with script...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/extra/test_v2x_suffr.R")
}
update_task_status(db = DB)