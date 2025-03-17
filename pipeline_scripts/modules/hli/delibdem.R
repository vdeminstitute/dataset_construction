#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_delibdem.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Functions
# --------------------------------------------------------------------------
calc_post <- function(matrix_list) {
	stopifnot(c("prob.delib", "post.polyarchy") %in% names(matrix_list),
		is.list(matrix_list),
		length(matrix_list) == 2)
	return(0.25 * (matrix_list[["post.polyarchy"]] ^ 1.585) + 
		0.25 * matrix_list[["prob.delib"]] +
    	0.5 * (matrix_list[["post.polyarchy"]] ^ 1.585) * matrix_list[["prob.delib"]])
}

main <- function(prob.delib, post.polyarchy, utable, TASK_NAME) {

	# Prepare BFA
	prob.delib %<>% bfa_stretch_z_sample(., utable)

	# Calculate index
	stretch_combined(named_list(prob.delib, post.polyarchy), utable) %>%
		calc_post(.)  %>%
		hli_summary(., TASK_NAME)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
	db <- pg_connect()
	get_globals()

	# Imports
	utable <- load_country_unit()
	objs <- find_dep_files(TASK_ID, db)

	prob.delib <- objs[["v2xdl_delib"]]
	post.polyarchy <- objs[["v2x_polyarchy"]][["v2x_polyarchy"]]$thin_post

	# Run
	collectedInputs <- named_list(prob.delib, post.polyarchy, utable, TASK_NAME)
	setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)
} else {
   	testthat::test_file("~/proj/vdemds/module_unit_tests/hli/test_delibdem.R")
}
