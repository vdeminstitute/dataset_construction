#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Cleaning script for variables that are cleaned by v2eltype and v3eltype.
# The variables that are cleaned and cleans are election-specific. Do not interpolate
# across missing values in this script.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------

check_cleaning_rule <- function(codebook, TASK_NAME) {

    rule <- codebook$date_specific
    stopifnot(length(rule) == 1)
    stopifnot(!is.na(rule))
    stopifnot(rule != "")
    stopifnot(grepl("Election|election", rule))
    
    return(rule)
}

merge_eltype <- function(df, eltype) {
    
    stopifnot(`df is not a data.frame` = is.data.frame(df))
    stopifnot(`eltype is not a data.frame` = is.data.frame(eltype))
    stopifnot("code" %in% names(df))

    eltype <- eltype[, c("country_id", "historical_date", "text_answer")]
    names(eltype) <- c("country_id", "historical_date", "eltype")
    stopifnot(!"text_answer" %in% names(eltype))

    df <- merge(
        x = df,
        y = eltype,
        by = c("country_id", "historical_date"),
        all.x = TRUE)
    stopifnot("eltype" %in% names(df))
    
    return(df)
}

# We remove observations in df where eltype is missing in eltype
remove_missing_election_dates <- function(df) {

    bool <- is.na(df[["eltype"]])
    stopifnot(`bool cannot have missingness` = !anyNA(bool))

    if (any(bool)) {
        df <- clean_observations(df, bool, "remove_missing_election_dates",
            "Observations dropped where eltype is missing")
    }

    stopifnot(`There should be no missing values left for eltype` =
        !anyNA(df[["eltype"]]))

    return(df)
}

# Helper function for remove_based_on_eltype_values
# -- it returns boolean values whether an observation should be removed
f <- function(df, eltype_, need_pattern, rule) {

    stopifnot(
        is.character(eltype_),
        is.character(need_pattern),
        is.character(rule))

    eltype_ <- sprintf("eltype_%s", eltype_)
    stopifnot(is.character(eltype_))

    # If the main pattern is found
    if (grepl(pattern = eltype_, x = rule, fixed = TRUE)) {

        # Remove if need_pattern is not matched
        bool <-
            !is.na(df[["eltype"]]) &
            !grepl(pattern = need_pattern, x = df[["eltype"]])
        
        stopifnot(`bool cannot have missingness` = !anyNA(bool))
        stopifnot(is.logical(bool) & length(bool) == nrow(df))

        return(bool)

    } else {
        return(rep(FALSE, times = nrow(df)))
    }
}

# Remove based on the values of eltype
remove_based_on_eltype_values <- function(df, rule) {
    
    stopifnot(`rule argument needs to a character vector` = is.character(rule))
    stopifnot(`df is not a data.frame` = is.data.frame(df))

    # List criterions
    bool_0_01 <- f(
        df = df,
        eltype_ = "0",
        need_pattern = "0|1",
        rule = rule)

    bool_2_23 <- f(
        df = df,
        eltype_ = "2",
        need_pattern = "2|3",
        rule = rule)

    bool_6_67 <- f(
        df = df,
        eltype_ = "6",
        need_pattern = "6|7",
        rule = rule)

    stopifnot(
        !is.null(bool_0_01),
        !is.null(bool_2_23),
        !is.null(bool_6_67)
        )

    stopifnot(
        length(bool_0_01) == nrow(df),
        length(bool_2_23) == nrow(df),
        length(bool_6_67) == nrow(df)
        )

    bool <- bool_0_01 | bool_2_23 | bool_6_67
    stopifnot(`bool cannot have missingness` = !anyNA(bool))
    stopifnot(is.logical(bool))

    if (any(bool)) {

        stopifnot(is.logical(bool) & length(bool) == nrow(df))

        df <- clean_observations(df, bool, "remove_based_on_eltype_values",
            "Observations dropped based on eltype values")

        }

    df[["eltype"]] <- NULL
    return(df)
}

main <- function(codebook, df, eltype, TASK_NAME) {

    rule <- check_cleaning_rule(codebook, TASK_NAME)
    df <- 
        merge_eltype(df, eltype) |>  
        remove_missing_election_dates(df=_) |>  
        remove_based_on_eltype_values(df = _, rule)

    return(df)
}


# Run script
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    codebook <- load_codebook()
    codebook <- codebook[codebook$tag == TASK_NAME, ]
    objs <- find_dep_files(TASK_ID, db)
    df <- objs[[TASK_NAME]][[TASK_NAME]]
    if (grepl("^v2", TASK_NAME)) {
        eltype <- objs[["v2eltype"]][["v2eltype"]]
    } else {
        eltype <- objs[["v3eltype"]][["v3eltype"]]
    }

    # Run
    collectedInputs <- named_list(codebook, df, eltype, TASK_NAME)
    setNames(list(do.call(main, collectedInputs), DIRTYLIST), 
        c(TASK_NAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
    info("Done with eltype_dates")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/downstream_cleaning/test_eltype_dates.R")
}
