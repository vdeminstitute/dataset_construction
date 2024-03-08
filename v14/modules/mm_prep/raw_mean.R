#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Calculate raw mean of nonreduced, interpolated C-vars.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(docopt))
suppressMessages(library(parallel))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
calc_means <- function(input.data, qtable, TASK_NAME) {
    
    # For raw means we subtract 1 (added in interpolate_coders) for all
    # variable types except R (percentage).
    subtract_bool <- qtable$question_type[qtable$name == TASK_NAME] == "R"

    if (!subtract_bool) {
        info("Subtracting 1 for raw means")
        input.data$wdata <- input.data$wdata - 1
    }

    # Find rowmeans
    m <- rowMeans(input.data$wdata, na.rm = TRUE)

    df <- data.frame(
        rownames = input.data$country_dates,
        x = m,
        stringsAsFactors = FALSE)
    colnames(df) <- c("rownames", TASK_NAME %^% "_mean")
    final.df <- df
    stopifnot(is.data.frame(final.df))

    return(final.df)
}

cd_df <- function(final.df) {
    stopifnot(is.data.frame(final.df))

    final_cd.df <- final.df %>%
        mutate(
            country_text_id = get_text_id(rownames),
            historical_date = get_date(rownames)) %>%
        select(country_text_id, historical_date, everything(), -rownames) %>%
        arrange(country_text_id, historical_date)
}

cy_df <- function(final_cd.df) {
    stopifnot(is.data.frame(final_cd.df))    
    # aggregate by day-weighted average
    # weights - number of days from the end of a year for a country.
    # set cores to 1 because this script is ran for parallel variables.
    final_cy.df <- final_cd.df %>%
        cy.day_mean(dates = historical_date, by = country_text_id, mc.cores = 1) %>%
        mutate_if(is.numeric, function(v) ifelse(is.nan(v), NA, v)) %>%
        select(country_text_id, year, everything())
}

final_cleaning <- function(qtable, final_cd.df, final_cy.df) {
    # rename if *not* mm or percent variable
    if (!qtable$mm && !qtable$percent) {
        names(final_cd.df) <- gsub("_mean", "", names(final_cd.df), fixed = TRUE)
        names(final_cy.df) <- gsub("_mean", "", names(final_cy.df), fixed = TRUE)
    }

    return(list(cy = final_cy.df, cd = final_cd.df))
}

main <- function(input.data, qtable, TASK_NAME) {
    final.df <- calc_means(input.data, qtable, TASK_NAME)
    final_cd.df <- cd_df(final.df)
    final_cy.df <- cy_df(final_cd.df)
    final_cleaning(qtable, final_cd.df, final_cy.df)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    qtable <- subset(load_qtable(), subset = name == TASK_NAME)
    stopifnot(nrow(qtable) == 1)
    input.data <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]

    # Run
    collectedInputs <- named_list(input.data, qtable, TASK_NAME)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done with calculating means for " %^% TASK_NAME)
} else {
    # Call unit tests for main function and sub functions
    testthat::test_file("~/proj/vdemds/module_unit_tests/mm_prep/test_raw_mean.R") %>%
		check_test_file_output()
}
