#!/usr/bin/env Rscript

# ==========================================================================
# Creates naive raw mean of nonreduced, interpolated C-vars. Reported
# in the final DS as v2*_mean.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(docopt))
suppressMessages(library(parallel))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "raw_mean")


#
# Functions
# --------------------------------------------------------------------------
calc_means <- function(input.data, qtable, varname) {
    # Remember, we added 1 for the MM so subtract when taking the raw
    # mean, except when dealing with percentage variables.
    if (!(varname %in% qtable$name &&
          qtable$question_type[qtable$name == varname] == "R"))
        input.data$wdata <- input.data$wdata - 1

    m <- rowMeans(input.data$wdata, na.rm = T)

    df <- data.frame(rownames = input.data$country_dates, x = m, stringsAsFactors = F)
    colnames(df) <- c("rownames", varname %^% "_mean")
    final.df <- df
}

cd_df <- function(final.df) {
    final_cd.df <- final.df %>%
        mutate(country_text_id = get_text_id(rownames), historical_date = get_date(rownames)) %>%
        select(country_text_id, historical_date, everything(), -rownames) %>%
        arrange(country_text_id, historical_date)
}

cy_df <- function(final_cd.df) {
    # no need in year column
    final_cy.df <- final_cd.df %>%
        # aggregate by day-weighted average
        # (weights - number of days from the end of a year for particular country_text_id);
        # the function uses mcMap but we set number of cores to 1 because this script
        # is ran for variables in parallel;
        cy.day_mean(dates = historical_date, by = country_text_id, mc.cores = 1) %>%
        mutate_if(is.numeric, function(v) ifelse(is.nan(v), NA, v)) %>%
        select(country_text_id, year, everything())
}

final_cleaning <- function(qtable, final_cd.df, final_cy.df) {
    # rename if not mm or percent variable
    if (!qtable$mm & !qtable$percent) {
        names(final_cd.df) <- gsub("_mean", "", names(final_cd.df), fixed = TRUE)
        names(final_cy.df) <- gsub("_mean", "", names(final_cy.df), fixed = TRUE)
    }

    return(list(cy = final_cy.df, cd = final_cd.df))
}

main <- function(input.data, qtable, VARNAME) {
    final.df <- calc_means(input.data, qtable, VARNAME)
    final_cd.df <- cd_df(final.df)
    final_cy.df <- cy_df(final_cd.df)
    final_cleaning(qtable, final_cd.df, final_cy.df)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    qtable <- load_qtable() %>%
        filter(name == VARNAME)
    input.data <- find_dep_files(TASK_ID, DB)[[VARNAME]][[VARNAME]]

    # Run
    collectedInputs <- named_list(input.data, qtable, VARNAME)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done with calculating means for " %^% VARNAME)
} else {
    # Call unit tests for main function and sub functions
    testthat::test_file("~/proj/mm-prep/tests/mm_prep/test_raw_mean.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)
