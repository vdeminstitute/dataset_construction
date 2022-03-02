#!/usr/bin/env Rscript

# ==========================================================================

options(warn = 2)
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "nonc_aggregate_cy")


#
# Functions
# --------------------------------------------------------------------------
merge_ref_tables <- function(df, country) {
    df %>% add_country_cols(country) %>%
        add_date_cols %>%
        select(-country_text_id) %>%
        return(.)
}

find_aggregation_method <- function(qtable, VARNAME) {
    stopifnot(!is.na(VARNAME), grepl("^v[2|3]", VARNAME))
    qtable %<>% filter(name == VARNAME)
   
    max_vars <- filter(qtable,
        (!is.na(date_specific) & (!question_type %in% c("R", "T"))) |
        (name %in% c("v3eldirelc", "v3eldireuc", "v3eldirepr")) |
        (name %~% "elecreg")) %>%
        pull(name)
    if (length(max_vars) > 0) {
		stopifnot(isTRUE(qtable$cy_aggregation == "Maximum"))
        return("max")
	}

    ratio_vars <- filter(qtable, question_type == "R",
        index_type == "interval",
        is.na(date_specific)) %>%
        pull(name)
    if (length(ratio_vars) > 0) {
		stopifnot(isTRUE(qtable$cy_aggregation == "Day-weighted mean"))
	    return("ratio")
	}   

	stopifnot(isTRUE(qtable$cy_aggregation == "Last"))
    return("last")
}

aggr_ratio <- function(df) {
    # We do parallelization on the level of the script,
    # so mc.cores for each variable is 1.
    cy.day_mean(df, historical_date, country_id,
        mc.cores = 1) %>%
    mutate(country_id = as.numeric(country_id)) %>%
    return(.)
}

aggr_max <- function(df) {
  # We could just precalculate these groups once
    df %>%
        select(-historical_date) %>%
        group_by(country_id, year) %>%
        summarise_all(collect_max) %>%
        ungroup() %>%
        as.data.frame() %>%
        return(.)
}

aggr_last <- function(df) {
    df %>%
        select(-historical_date) %>%
        group_by(country_id, year) %>%
        summarise_all(collect_last) %>%
        ungroup() %>%
        as.data.frame() %>%
        return(.)
}


main <- function(df, VARNAME, qtable, country) {
    info("Aggregating to CY")
    df <- merge_ref_tables(df, country)
    aggregation_method <- find_aggregation_method(qtable, VARNAME)

    if (aggregation_method == "max") {
        df_cy <- aggr_max(df)
    } else if (aggregation_method == "ratio") {
        df_cy <- aggr_ratio(df)
    } else {
        df_cy <- aggr_last(df)
    }
    with(df_cy, stopifnot(!is.na(country_id), !is.na(year)))
    return(list(cd = df, cy = df_cy))
}

# Run script
if (no_test()) {
    # Global variables
    get_globals()
    dirtylist <- list()

    # Imports
    qtable <- load_qtable()
    country <- load_country()
    objs <- find_dep_files(TASK_ID, DB)
    df <- objs[[VARNAME]][[VARNAME]]
    
    # Run
    collectedInputs <- named_list(df, VARNAME, qtable, country)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with nonc_aggregate_cy...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/init/test_nonc_aggregate_cy.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)