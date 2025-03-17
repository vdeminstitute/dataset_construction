#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# This script performs an inner join between CD/CY and utable using
# country_id and year as keys.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------

clean_by_utable <- function(df, utable) {

	YEAR_ADDED <- FALSE
	if (!"year" %in% names(df)) {
		df$year <- to_year(df$historical_date)
		YEAR_ADDED <- TRUE
	}
	stopifnot(`country_text_id column is missing!` = 
		"country_id" %in% names(df))
    
    # inner join
    outdf <- merge(df, utable[, c("country_id", "year")],
        by = c("country_id", "year"))
    
    # Define observations that are not in outdf but in df
    dfFactor <- interaction(df[, c("country_id", "year")], drop = FALSE)
    outdfFactor <- interaction(outdf[, c("country_id", "year")], drop = FALSE)
    removedObservations <- organiseRows(df[!dfFactor %in% outdfFactor,], country_id, year)

    DIRTYLIST[["clean_by_utable"]][[deparse(substitute(df))]] <<- removedObservations

	if (YEAR_ADDED) {
		outdf$year <- NULL
	}

	return(outdf)
}

main <- function(utable, cd, cy) {
	df_cd <- organiseRows(clean_by_utable(cd, utable), country_id, historical_date)
    df_cy <- organiseRows(clean_by_utable(cy, utable), country_id, year)
    return(list(cd = df_cd, cy = df_cy))
}

# Run script
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
	utable <- load_country_unit()[, c("country_id", "year")]
	objs <- find_dep_files(TASK_ID, db)
    cd <- objs[[TASK_NAME]][[TASK_NAME]]$cd
	cy <- objs[[TASK_NAME]][[TASK_NAME]]$cy

    # Run
    collectedInputs <- named_list(utable, cd, cy)
    setNames(list(do.call(main, collectedInputs), DIRTYLIST), 
        c(TASK_NAME, "dirt")) %>% 
        write_file(., OUTFILE, dir_create = TRUE)    
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_clean_by_utable.R") %>%
		check_test_file_output()
}
