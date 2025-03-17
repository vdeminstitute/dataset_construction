#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_gender.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
calc_post <- function(matrix_list) {
	stopifnot(c("prob.gencl", "prob.gencs", "prob.genpp") %in% names(matrix_list),
		is.list(matrix_list),
		length(matrix_list) == 3)
	return((matrix_list[["prob.gencl"]] + matrix_list[["prob.gencs"]] + 
		matrix_list[["prob.genpp"]]) / 3)
}

main <- function(prob.gencl, prob.gencs, prob.genpp, utable, TASK_NAME) {
	prob.gencl %<>% bfa_stretch_z_sample(., utable)
	prob.gencs %<>% bfa_stretch_z_sample(., utable)

	stretch_combined(named_list(prob.gencl, prob.gencs, prob.genpp), utable) %>%
		calc_post(.) %>%
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
	# BFA
	prob.gencl <- objs[["v2x_gencl"]]
	prob.gencs <- objs[["v2x_gencs"]]
	# HLI
	prob.genpp <- objs[["v2x_genpp"]][["v2x_genpp"]]$thin_post

	# Run
	collectedInputs <- named_list(prob.gencl, prob.gencs, prob.genpp, utable, TASK_NAME)
	setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)

	} else {
    	testthat::test_file("~/proj/vdemds/module_unit_tests/hli/test_gender.R")
	}
