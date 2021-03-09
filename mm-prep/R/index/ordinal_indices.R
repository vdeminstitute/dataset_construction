#!/usr/bin/env Rscript

# ==========================================================================
# Construct ordinal indices
# ==========================================================================
options(warn = 2)

suppressMessages(library(vutils))
suppressMessages(library(dplyr))
suppressMessages(library(docopt))
set_env(MODULE_NAME = "ordinal_indices")

#
# Functions
# --------------------------------------------------------------------------
ordinalise <- function(df, vname) {
	df[[sprintf("e_%s_3C", vname)]] <- ord_3C(df[[vname]])
	df[[sprintf("e_%s_4C", vname)]] <- ord_4C(df[[vname]])
	df[[sprintf("e_%s_5C", vname)]] <- ord_5C(df[[vname]])
	return(df)
}

main <- function(df, vname) {
	df <- ordinalise(df, vname) %>%
		select(country_id, country_text_id, historical_date, year,
			matches("\\dC$", ignore.case = FALSE))
	return(list(cy = df))
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
	get_globals()

	# Imports
	country <- load_country()
	objs <- find_dep_files(TASK_ID, DB)
	vname <- gsub("^e_", "", VARNAME)

	df <- objs[[vname]][[vname]]$cy %>%
	    add_country_cols(country) %>%
	    add_date_cols
	
	info("Ordinalizing: " %^% vname)

	# Run
	collectedInputs <- named_list(df, vname)
	setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)
	} else {
    	testthat::test_file("~/proj/mm-prep/tests/index/test_ordinal_indices.R")	
	}
update_task_status(db = db)
