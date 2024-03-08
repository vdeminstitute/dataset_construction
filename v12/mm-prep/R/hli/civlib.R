#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_civlib
# ==========================================================================

options(warn = 2)

suppressMessages(library(docopt))
suppressMessages(library(dplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "v2x_civlib")

#
# Functions
# --------------------------------------------------------------------------
calc_civlib <- function(matrix_list) {
	stopifnot(c("prob.clpol", "prob.clphy", "prob.clpriv") %in% names(matrix_list),
		is.list(matrix_list),
		length(matrix_list) == 3)
	post.civlib <- (matrix_list[["prob.clphy"]] + matrix_list[["prob.clpol"]] + 
		matrix_list[["prob.clpriv"]]) / 3
	return(post.civlib)
}

main <- function(prob.clpol, prob.clphy, prob.clpriv, utable, VARNAME) {
    # Stretch components individually
    prob.clphy %<>% binary_stretch_z_sample(., utable) %>% pnorm
    prob.clpol %<>% bfa_stretch_z_sample(., utable)
    prob.clpriv %<>% bfa_stretch_z_sample(., utable)
    # Stretch to combined dates
	stretch_combined(named_list(prob.clpol, prob.clphy, prob.clpriv), utable) %>%
    # Calculate index
		calc_civlib(.) %>%
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
	objs <- find_dep_files(Sys.getenv("TASK_ID"), db)
	# Binary index
	prob.clphy <- objs[["v2x_clphy"]]
	# BFAs
	prob.clpol  <- objs[["v2x_clpol"]]
	prob.clpriv <- objs[["v2x_clpriv"]]

	# Run
	collectedInputs <- named_list(prob.clpol, prob.clphy, prob.clpriv, utable, VARNAME)
	setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)
} else {
    testthat::test_file("~/proj/mm-prep/tests/hli/test_civlib.R")
}
update_task_status(db = DB)
