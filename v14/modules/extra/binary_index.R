#!/usr/bin/env Rscript

# ==========================================================================
# R script for binary indices (i.e. indices constructed from only two
# input variables) Aggregation is done by first normalizing the draws from the
# z.sample csv files (posterior 50% sample file from the MM) and then
# taking the raw mean.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
union_country_dates <- function(x, y) {
	ucd <- union(rownames(x), rownames(y))
	info(sprintf("Found %d combined country-dates", length(ucd)))
	return(ucd)
}

# Stretch both matrices to cover the full set of dates
stretch_and_combine_matrices <- function(x, y, utable) {

    info(sprintf("Number of rows in [x: %d], [y: %d]", nrow(x), nrow(y)))

    # Union of country-dates
    dates <- union_country_dates(x, y)

    # Stretch both matrices to cover the full set of dates
    x <- stretch(
        x = x,
        by = dates,
        gaps = TRUE,
        rule_366 = FALSE,
        utable = utable)
	y <- stretch(
        x = y,
        by = dates,
        gaps = TRUE,
        rule_366 = FALSE,
        utable = utable)

    info(sprintf("Number of rows after stretching [x: %d], [y: %d]",
        nrow(x), nrow(y)))

    # Mean of the two matrices
	res <- (x + y) / 2

	return(res)
}

# Turn into a country-date data.frame
# -- we use pnorm to normalize the values to [0,1] scale
cd_df <- function(combined, full_names, utable) {
	cd.df <-
        dist_summary(t(combined), full_names, utable = utable) %>%
        mutate_if(is.numeric, pnorm) %>%
        as.data.frame()
}

# Aggregate using day-weighted mean
cy_df <- function(cd.df) {
	cy.df <- cy.day_mean(cd.df, historical_date, country_text_id)
}

main <- function(x, y, utable, input_names, TASK_NAME) {

    info(sprintf("Averaging inputs for %s", TASK_NAME))
	
    combined <- stretch_and_combine_matrices(x, y, utable)
	cd.df <- cd_df(combined, input_names, utable)
	cy.df <- cy_df(cd.df)
	
    return(list(post.sample = list(z = combined), 
		cd = fix_stat_columns(cd.df, TASK_NAME),
		cy = fix_stat_columns(cy.df, TASK_NAME),
		country_dates = input_names))
}

#
# Load data
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
    db <- pg_connect()
	get_globals()
    stopifnot(substring(TASK_NAME, 1, 3) == "v2x")

	# Imports
	objs <- find_dep_files(TASK_ID, db)
	utable <- load_country_unit()
	v1 <- names(objs)[1]
	v2 <- names(objs)[2]

	# Load the z sample file for each variable
    # -- standardize each observation to the moments of the matrix
	x <- scale(load_matrix(objs[[v1]][[v1]]$post.sample$z, drop.vignettes = TRUE))
	y <- scale(load_matrix(objs[[v2]][[v2]]$post.sample$z, drop.vignettes = TRUE))
	input_names <- union(objs[[v1]][[v1]]$country_dates, objs[[v2]][[v2]]$country_dates)
	
    # Run
	collectedInputs <- named_list(x, y, utable, input_names, TASK_NAME)
	setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)

	} else {
		# Call unit tests for main function and sub functions
        testthat::test_file("~/proj/vdemds/module_unit_tests/extra/test_binary_index.R")
	}
