#!/usr/bin/env Rscript

# This script inverts the index scores for certain indices.
# It is applied / can be applied to binary indices and BFAs.
# The underlying z-samples are not inverted in this script
# The z-samples are inverted when calculating an index.


suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))


# Functions
# --------------------------------------------------------------------------
# We need to switch the codehigh and codelow values for the flipped indices:
switch_cols <- function(df, i) {
       df[, c(i %^% "_codelow68", i %^% "_codehigh68")] <-
           df[, c(i %^% "_codehigh68", i %^% "_codelow68")]
       df[, c(i %^% "_codelow95", i %^% "_codehigh95")] <-
           df[, c(i %^% "_codehigh95", i %^% "_codelow95")]
       return(df)
}

# The "flip it" pipe operator
pipe_ <- . %>%
    mutate_at(vars(matches(TASK_NAME)), function(v) 1 - v) %>%
    mutate_at(vars(matches("codehigh")), function(v) pmin(v, 1)) %>%
    mutate_at(vars(matches("codelow")), function(v) pmax(v, 0))

flip_bfa <- function(object, lev = c("cd", "cy"), TASK_NAME) {
    lev <- match.arg(lev)
    df <- object[[lev]] %>%
        pipe_ %>% switch_cols(i = TASK_NAME)
    return(df)
}

main <- function(object, TASK_NAME) {
    # Apply the flipping function
    object[["cd"]] <- flip_bfa(object, lev = "cd", TASK_NAME)
    object[["cy"]] <- flip_bfa(object, lev = "cy", TASK_NAME)
    return(object)
}


# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Gelobal variables
    db <- pg_connect()
    get_globals()
    
    # Imports
    objs <- find_dep_files(TASK_ID, db)
    object <- objs[[TASK_NAME]][[TASK_NAME]]

    # Run
    collectedInputs <- named_list(object, TASK_NAME)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/bfa/test_flip_bfa.R")
}
