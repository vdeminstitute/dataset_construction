#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_corr.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
calc_corr <- function(comp_list) {
	# v2lgcrrpt is missing when v2lgbicam == 0 
	post.corr <- (comp_list[["prob.execorr"]] + comp_list[["prob.pubcorr"]] + 
		comp_list[["prob.v2lgcrrpt"]] + comp_list[["prob.v2jucorrdc"]]) / 4
	post.icorr <- (comp_list[["prob.execorr"]] + comp_list[["prob.pubcorr"]] + 
		comp_list[["prob.v2jucorrdc"]]) / 3

	post.corr[is.na(post.corr[, 1]), ] <- post.icorr[is.na(post.corr[, 1]), ]
	return(post.corr)
}

main <- function(execorr, pubcorr, v2lgcrrpt, v2jucorrdc, utable, TASK_NAME) {
	# Stretch individually
	execorr %<>% binary_stretch_z_sample(., utable)
	pubcorr %<>% binary_stretch_z_sample(., utable)
	v2lgcrrpt %<>% mm_stretch_z_sample(., utable)
	v2jucorrdc %<>% mm_stretch_z_sample(., utable)

	# Pnorm and invert scale to match BFAs
	prob.execorr <- 1 - pnorm(execorr)
	prob.pubcorr <- 1 - pnorm(pubcorr)
	prob.v2lgcrrpt <- 1 - pnorm(v2lgcrrpt)
	prob.v2jucorrdc <- 1 - pnorm(v2jucorrdc)

	# Stretch to combined dates and calculate index
	stretch_combined(named_list(prob.execorr, prob.pubcorr, 
								prob.v2lgcrrpt, prob.v2jucorrdc), utable) %>%
		calc_corr(.) %>%
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
	# Extract binary index components
	execorr <- objs[["v2x_execorr"]]
	pubcorr <- objs[["v2x_pubcorr"]]
	# Extract MM components
	v2lgcrrpt <- objs[["v2lgcrrpt"]]
	v2jucorrdc <- objs[["v2jucorrdc"]]

	# Run
	collectedInputs <- named_list(execorr, pubcorr, v2lgcrrpt, v2jucorrdc, utable, TASK_NAME)
	setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)
	} else {
    	testthat::test_file("~/proj/vdemds/module_unit_tests/hli/test_corr.R")
	}
