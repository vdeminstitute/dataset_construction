#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_libdem.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Functions
# --------------------------------------------------------------------------
calc_libdem <- function(matrix_list) {

	stopifnot(
        c("post.polyarchy", "post.liberal") %in% names(matrix_list),
		is.list(matrix_list),
		length(matrix_list) == 2)
    
	return(0.25 * (matrix_list[["post.polyarchy"]] ^ 1.585) + 
		0.25 * matrix_list[["post.liberal"]] +
    	0.5 * (matrix_list[["post.polyarchy"]] ^ 1.585) * matrix_list[["post.liberal"]])
}

main <- function(post.polyarchy, post.liberal, utable, TASK_NAME) {
	
    ll <- stretch_combined(
        named_list(post.polyarchy, post.liberal),
        utable)

    out <- hli_summary(calc_libdem(ll), TASK_NAME)
    return(out)
}

#
# Write output
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    utable <- load_country_unit()
    objs <- find_dep_files(TASK_ID, db)
	# HLIs
    post.polyarchy <- objs[["v2x_polyarchy"]][["v2x_polyarchy"]]$thin_post
	post.liberal <- objs[["v2x_liberal"]][["v2x_liberal"]]$thin_post

	# Run
	collectedInputs <- named_list(post.polyarchy, post.liberal, utable, TASK_NAME)
	setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)

} else {
	# Call unit tests for main function and sub functions
    testthat::test_file("~/proj/vdemds/module_unit_tests/hli/test_libdem.R")
}
