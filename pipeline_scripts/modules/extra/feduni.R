#!/usr/bin/env Rscript

# --------------------------------------------------------------------------
# Generate v2x_feduni (Division of power index)
# --------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
make_feduni <- function(df) {
    feduni <-
        df %>%
        mutate(
            v2ellocpwr = pnorm(v2ellocpwr),
            v2elrgpwr = pnorm(v2elrgpwr),
            v2elsrgel = case_when(
                is.na(v2elreggov) | v2elreggov == 0 ~ 0,
                v2elsrgel %in% c(4, 5) ~ 2,
                v2elsrgel == 3 ~ 1,
                TRUE ~ v2elsrgel) / 2,
            v2ellocelc = case_when(
                is.na(v2ellocgov) | v2ellocgov == 0 ~ 0,
                v2ellocelc %in% c(4, 5) ~ 2,
                v2ellocelc == 3 ~ 1,
                TRUE ~ v2ellocelc) / 2) %>%
        mutate(v2ellocpwr = ifelse(is.na(v2ellocgov) | is.na(v2ellocpwr), 0, v2ellocpwr),
               v2elrgpwr = ifelse(is.na(v2elreggov) | is.na(v2elrgpwr), 0, v2elrgpwr),
               v2x_feduni = 0.5 * v2ellocgov * v2ellocelc * v2ellocpwr +
                            0.5 * v2elreggov * v2elsrgel * v2elrgpwr) %>%
        select(country_text_id, year, v2x_feduni)
    return(feduni)
}

main <- function(df) {
    make_feduni(df)
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
    objs <- find_dep_files(TASK_ID, db)

    df <- Reduce(full_join_vdem,
        lapply(names(objs), function(v) add_country_cols(objs[[v]][[v]]$cy, country)))

    # Run
    main(df) %>%
        list(cy = .) %>%
        list(.) %>%
        setNames(TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)

    } else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/extra/test_feduni.R")
    }
