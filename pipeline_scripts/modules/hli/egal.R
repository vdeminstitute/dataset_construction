#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_egal.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------

calc_post <- function(prob.eqdr, prob.eqprotec, prob.eqaccess) {
	return((prob.eqprotec + prob.eqdr + prob.eqaccess) / 3)
}

main <- function(prob.eqdr, prob.eqprotec, prob.eqaccess, utable, TASK_NAME) {
	
	# Stretch individual components
	prob.eqdr %<>% bfa_stretch_z_sample(., utable)
	prob.eqprotec %<>% bfa_stretch_z_sample(., utable)
	prob.eqaccess %<>% bfa_stretch_z_sample(., utable)
	
	# Stretch to combined dates and calculate index
	ll <- stretch_combined(named_list(prob.eqdr, prob.eqprotec, prob.eqaccess), utable)
	calc_post(ll$prob.eqdr, ll$prob.eqprotec, ll$prob.eqaccess) %>%
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
	# Load BFAs
	prob.eqdr <- objs[["v2xeg_eqdr"]]
	prob.eqprotec <- objs[["v2xeg_eqprotec"]]
	prob.eqaccess <- objs[["v2xeg_eqaccess"]]

	# Run
	collectedInputs <- named_list(prob.eqdr, prob.eqprotec, prob.eqaccess, utable, TASK_NAME)
	setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)

	} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/hli/test_egal.R")
	}
