#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_egal.
# ==========================================================================

options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "v2x_egal")

#
# Functions
# --------------------------------------------------------------------------

calc_post <- function(prob.eqdr, prob.eqprotec, prob.eqaccess) {
	return((prob.eqprotec + prob.eqdr + prob.eqaccess) / 3)
}

main <- function(prob.eqdr, prob.eqprotec, prob.eqaccess, utable, VARNAME) {
	
	# Stretch individual components
	prob.eqdr %<>% bfa_stretch_z_sample(., utable)
	prob.eqprotec %<>% bfa_stretch_z_sample(., utable)
	prob.eqaccess %<>% bfa_stretch_z_sample(., utable)
	
	# Stretch to combined dates and calculate index
	ll <- stretch_combined(named_list(prob.eqdr, prob.eqprotec, prob.eqaccess), utable)
	calc_post(ll$prob.eqdr, ll$prob.eqprotec, ll$prob.eqaccess) %>%
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
	# Load BFAs
	prob.eqdr <- objs[["v2xeg_eqdr"]]
	prob.eqprotec <- objs[["v2xeg_eqprotec"]]
	prob.eqaccess <- objs[["v2xeg_eqaccess"]]

	# Run
	collectedInputs <- named_list(prob.eqdr, prob.eqprotec, prob.eqaccess, utable, VARNAME)
	setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)

	} else {
	# Call unit tests for main function and sub functions
    testthat::test_file("~/proj/mm-prep/tests/hli/test_egal.R")
	}
update_task_status(db = DB)
