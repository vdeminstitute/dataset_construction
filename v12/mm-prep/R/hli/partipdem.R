#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_partipdem.
# ==========================================================================

options(warn = 2)

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "v2x_partipdem")

#
# Functions
# --------------------------------------------------------------------------
calc_partipdem <- function(matrix_list) {
	stopifnot(c("post.polyarchy", "post.partip") %in% names(matrix_list),
		is.list(matrix_list),
		length(matrix_list) == 2)
	return(0.25 * (matrix_list[["post.polyarchy"]] ^ 1.585) + 
		0.25 * matrix_list[["post.partip"]] +
    	0.5 * (matrix_list[["post.polyarchy"]] ^ 1.585) * matrix_list[["post.partip"]])
}

main <- function(post.polyarchy, post.partip, utable, VARNAME) {
	stretch_combined(named_list(post.polyarchy, post.partip), utable) %>%
		calc_partipdem(.) %>%
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
	# HLIs
	post.polyarchy <- objs[["v2x_polyarchy"]][["v2x_polyarchy"]]$thin_post %>%
	    load_matrix
	post.partip <- objs[["v2x_partip"]][["v2x_partip"]]$thin_post %>%
	    load_matrix

	# Run
	collectedInputs <- named_list(post.polyarchy, post.partip, utable, VARNAME)
	setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
} else {
    testthat::test_file("~/proj/mm-prep/tests/hli/test_partipdem.R")
}
update_task_status(db = DB)
