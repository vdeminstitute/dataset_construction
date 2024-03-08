#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_libdem.
# ==========================================================================

options(warn = 2)


suppressMessages(library(dplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "v2x_libdem")

#
# Functions
# --------------------------------------------------------------------------
calc_libdem <- function(matrix_list) {
	stopifnot(c("post.polyarchy", "post.liberal") %in% names(matrix_list),
		is.list(matrix_list),
		length(matrix_list) == 2)
	return(0.25 * (matrix_list[["post.polyarchy"]] ^ 1.585) + 
		0.25 * matrix_list[["post.liberal"]] +
    	0.5 * (matrix_list[["post.polyarchy"]] ^ 1.585) * matrix_list[["post.liberal"]])
}

main <- function(post.polyarchy, post.liberal, utable, VARNAME) {
	stretch_combined(named_list(post.polyarchy, post.liberal), utable) %>%
		calc_libdem(.) %>%
		hli_summary(., VARNAME)
}

#
# Write output
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    utable <- load_country_unit()
    objs <- find_dep_files(TASK_ID, DB)
	# HLIs
    post.polyarchy <- objs[["v2x_polyarchy"]][["v2x_polyarchy"]]$thin_post
	post.liberal <- objs[["v2x_liberal"]][["v2x_liberal"]]$thin_post

	# Run
	collectedInputs <- named_list(post.polyarchy, post.liberal, utable, VARNAME)
	setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)

} else {
	# Call unit tests for main function and sub functions
    testthat::test_file("~/proj/mm-prep/tests/hli/test_libdem.R")
}
update_task_status(db = DB)