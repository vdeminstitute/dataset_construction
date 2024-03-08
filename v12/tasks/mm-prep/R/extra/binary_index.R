#!/usr/bin/env Rscript

# ==========================================================================
#
# R script for binary indices (i.e. indices constructed from only two
# input variables) --- for example, the male & female
# indices. Aggregation is done by first normalizing the draws from the
# z.sample csv files (posterior 50% sample file from the MM) and then
# taking the raw mean.
# ==========================================================================

options(warn = 2)

suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "binary")

#
# Functions
# --------------------------------------------------------------------------
combine_names <- function(x.ma, y.ma) {
	combined_names <- union(rownames(x.ma), rownames(y.ma))
	sprintf("Found %d combined country-dates", length(combined_names)) %>% info
	return(combined_names)
}

make_full_ma <- function(x.ma, y.ma, combined_names, utable) {
	x.ma %<>% stretch(combined_names, gaps = TRUE, rule_366 = FALSE, utable = utable)
	y.ma %<>% stretch(combined_names, gaps = TRUE, rule_366 = FALSE, utable = utable)

	full.ma <- (x.ma + y.ma) / 2

	return(full.ma)
}

cd_df <- function(full.ma, full_names, utable) {
	# Transform the final point estimates by the normal CDF so that we
	# match the BFAs and have everything scaled from 0 - 1.
	cd.df <- dist_summary(t(full.ma), full_names, utable = utable) %>%
	    mutate_if(is.numeric, pnorm) %>%
	    as.data.frame
}

cy_df <- function(cd.df) {
	cy.df <- cy.day_mean(cd.df, historical_date, country_text_id)
}

main <- function(x.ma, y.ma, utable, input_names, VARNAME) {
	combined_names <- combine_names(x.ma, y.ma)
	full.ma <- make_full_ma(x.ma, y.ma, combined_names, utable)
	cd.df <- cd_df(full.ma, input_names, utable)
	cy.df <- cy_df(cd.df)
	return(list(post.sample = list(z = full.ma), 
		cd = fix_stat_columns(cd.df, VARNAME),
		cy = fix_stat_columns(cy.df, VARNAME),
		country_dates = input_names))
}

#
# Load data
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
	get_globals()

	# Imports
	objs <- find_dep_files(TASK_ID, DB)

	v1 <- names(objs)[1]
	v2 <- names(objs)[2]

	# Load each dependency and merge
	X <- objs[[v1]][[v1]]$post.sample$z
	Y <- objs[[v2]][[v2]]$post.sample$z
	input_names <- union(objs[[v1]][[v1]]$country_dates,
                        objs[[v2]][[v2]]$country_dates)
	stopifnot(substring(VARNAME, 1, 3) == "v2x")

	info("Constructing " %^% VARNAME)

	utable <- load_country_unit()

	# Read in each z.sample file, add empty gap years, conform the starts
	# to the official utable, and normalize values.
	x.ma <- load_matrix(X, drop.vignettes = TRUE) %>%
	    scale

	y.ma <- load_matrix(Y, drop.vignettes = TRUE) %>%
	    scale

	# Run
	collectedInputs <- named_list(x.ma, y.ma, utable, input_names, VARNAME)
	setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)

	info("Finished with making binary index for " %^% VARNAME)

	} else {
		# Call unit tests for main function and sub functions
    	testthat::test_file("~/proj/mm-prep/tests/extra/test_binary_index.R")
	}
update_task_status(db = DB)