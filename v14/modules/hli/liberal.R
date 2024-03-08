#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_liberal.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Functions
# --------------------------------------------------------------------------
calc_liberal <- function(matrix_list) {

	stopifnot(
        c("prob.rol", "prob.jucon", "prob.legcon", "v2lgbicam") %in%
            names(matrix_list),
		is.list(matrix_list),
		length(matrix_list) == 4)

	# Set legcon to 0 where v2lgibcam is 0
	matrix_list[["prob.legcon"]][matrix_list[["v2lgbicam"]] == 0, ] <- 0
	
	post.liberal <-
        (matrix_list[["prob.rol"]] + matrix_list[["prob.jucon"]] + matrix_list[["prob.legcon"]]) / 3
    
    return(post.liberal)
}

main <- function(lgbicam, prob.rol, prob.jucon, prob.legcon, utable, country, TASK_NAME) {
	
    # Stretch BFA across country-dates
    prob.rol <- bfa_stretch_z_sample(prob.rol, utable)
	prob.jucon <- bfa_stretch_z_sample(prob.jucon, utable)
	prob.legcon <- bfa_stretch_z_sample(prob.legcon, utable)

    # Prepare lgbicam
    extra <- as.data.frame(
        clean_by_utable(add_country_cols(lgbicam, country), utable),
        stringsAsFactors = FALSE)

	rownames(extra) <- with(extra, paste(country_text_id, historical_date))
	v2lgbicam <- data.matrix(extra[, "v2lgbicam", drop = FALSE])

	ll <-
        stretch_combined(
            named_list(v2lgbicam, prob.rol, prob.jucon, prob.legcon), 
            utable)
    
    out <- hli_summary(calc_liberal(ll), TASK_NAME)
    return(out)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
    db <- pg_connect()
	get_globals()

	# Imports
	country <- load_country()
	utable <- load_country_unit()
	objs <- find_dep_files(TASK_ID, db)
	lgbicam <- objs[["v2lgbicam"]][["v2lgbicam"]]$cd
	# BFAs
	prob.rol <- objs[["v2xcl_rol"]]
	prob.jucon <- objs[["v2x_jucon"]]
	prob.legcon <- objs[["v2xlg_legcon"]]
	# Run
	collectedInputs <- named_list(lgbicam, prob.rol, prob.jucon, prob.legcon, 
        utable, country, TASK_NAME)
	setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)

} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/hli/test_liberal.R")
}
