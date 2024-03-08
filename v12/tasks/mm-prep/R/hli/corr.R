#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_corr.
# ==========================================================================

options(warn = 2)

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "v2x_corr")

#
# Functions
# --------------------------------------------------------------------------
calc_corr <- function(comp_list) {
	# v2lgcrrpt is missing when v2lgbicam == 0 so for now we keep the old
	# "thin" imputation method.
	post.corr <- (comp_list[["prob.execorr"]] + comp_list[["prob.pubcorr"]] + 
		comp_list[["prob.v2lgcrrpt"]] + comp_list[["prob.v2jucorrdc"]]) / 4
	post.icorr <- (comp_list[["prob.execorr"]] + comp_list[["prob.pubcorr"]] + 
		comp_list[["prob.v2jucorrdc"]]) / 3

	post.corr[is.na(post.corr[, 1]), ] <- post.icorr[is.na(post.corr[, 1]), ]
	return(post.corr)
}

main <- function(execorr, pubcorr, v2lgcrrpt, v2jucorrdc, utable, VARNAME) {

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
		hli_summary(., VARNAME)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
	get_globals()

	# Imports
	utable <- load_country_unit()
	objs <- find_dep_files(TASK_ID, DB)
	# Extract binary index components
	execorr <- objs[["v2x_execorr"]]
	pubcorr <- objs[["v2x_pubcorr"]]
	# Extract MM components
	v2lgcrrpt <- objs[["v2lgcrrpt"]]
	v2jucorrdc <- objs[["v2jucorrdc"]]

	# Run
	collectedInputs <- named_list(execorr, pubcorr, v2lgcrrpt, v2jucorrdc, utable, VARNAME)
	setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)
	} else {
    	testthat::test_file("~/proj/mm-prep/tests/hli/test_corr.R")
	}
update_task_status(db = DB)
