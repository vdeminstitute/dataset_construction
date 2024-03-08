#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_liberal.
# ==========================================================================

options(warn = 2)

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "v2x_liberal")


#
# Functions
# --------------------------------------------------------------------------
calc_liberal <- function(matrix_list) {
	stopifnot(c("prob.rol", "prob.jucon", "prob.legcon", "v2lgbicam") %in% names(matrix_list),
		is.list(matrix_list),
		length(matrix_list) == 4)

	# Set legcon to 0 where v2lgibcam is 0
	matrix_list[["prob.legcon"]][matrix_list[["v2lgbicam"]] == 0, ] <- 0
	
	post.liberal <- (matrix_list[["prob.rol"]] + matrix_list[["prob.jucon"]] + 
		matrix_list[["prob.legcon"]]) / 3
		return(post.liberal)
}

main <- function(lgbicam, prob.rol, prob.jucon, prob.legcon, utable, country, VARNAME) {
	prob.rol %<>% bfa_stretch_z_sample(., utable)
	prob.jucon %<>% bfa_stretch_z_sample(., utable)
	prob.legcon %<>% bfa_stretch_z_sample(., utable)

	extra <- lgbicam %>%
	    add_country_cols(country) %>%
	    clean_by_utable(utable) %>%
	    as.data.frame(stringsAsFactors = F)
	rownames(extra) <- with(extra, paste(country_text_id, historical_date))
	v2lgbicam <- data.matrix(extra[, "v2lgbicam", drop = F])

	stretch_combined(named_list(v2lgbicam, prob.rol, prob.jucon, prob.legcon), 
				     utable) %>%
		calc_liberal(.) %>%
		hli_summary(., VARNAME)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
	get_globals()

	# Imports
	country <- load_country()
	utable <- load_country_unit()
	objs <- find_dep_files(TASK_ID, DB)
	lgbicam <- objs[["v2lgbicam"]][["v2lgbicam"]]$cd
	# BFAs
	prob.rol <- objs[["v2xcl_rol"]]
	prob.jucon <- objs[["v2x_jucon"]]
	prob.legcon <- objs[["v2xlg_legcon"]]

	# Run
	collectedInputs <- named_list(lgbicam, prob.rol, prob.jucon, prob.legcon, 
								  utable, country, VARNAME)
	setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)

} else {
    testthat::test_file("~/proj/mm-prep/tests/hli/test_liberal.R")
}
update_task_status(db = DB)
