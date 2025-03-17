#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# The This script creates the v2x_divparctrl index.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
# Divided party control of legislature
divpartcl <- function(df) {

    df[["v2x_divparctrl"]] <-
        as.vector(scale(ifelse(
            test = !is.na(df[["v2psnatpar_ord"]]) & df[["v2psnatpar_ord"]] == 2,
            yes = df[["v2psnatpar"]] - 5,
            no = df[["v2psnatpar"]])))
    
    return(df[, c("v2x_divparctrl", "historical_date", "country_text_id")])

}

main <- function(df) {
    divpartcl(df)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()
    db <- pg_connect()

    # Imports
    objs <- find_dep_files(TASK_ID, db)
    df <- Reduce(
        partial(full_join, by = c("country_id", "historical_date", "year")),
        lapply(names(objs), function(v) {
            objs[[v]][[v]]$cd
        }))

    # Run
    setNames(list(main(df)), TASK_NAME) |> 
        write_file(o=_, OUTFILE, dir_create = TRUE)

    } else {
        testthat::test_file("~/proj/vdemds/module_unit_tests/extra/v2x_divparctrl.R")
    }
