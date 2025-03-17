#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# cleaned by a-star v2exnamhos and v3exnamhos
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
check_date_specific_rule <- function(codebook, TASK_NAME) {

    stopifnot(`TASK_NAME needs to be of class character` = is.character(TASK_NAME))
    stopifnot(`TASK_NAME needs to be of length 1` = length(TASK_NAME) == 1)
    stopifnot(`TASK_NAME cannot be an empty character vector` = TASK_NAME != "")

    rule <- codebook[["date_specific"]]
    stopifnot(length(rule) == 1)
    stopifnot(!is.na(rule))
    stopifnot(rule != "")
    stopifnot(grepl("HOS appointment dates", fixed = TRUE, x = rule))
}

merge_exnamhos <- function(df, exnamhos) {

    stopifnot("code" %in% names(df))

    exnamhos <- exnamhos[, c("country_id", "historical_date", "text_answer")]
    names(exnamhos) <- c("country_id", "historical_date", "exnamhos")
    stopifnot(!"text_answer" %in% names(exnamhos))
    
    df <- merge(
        x = df,
        y = exnamhos,
        by = c("country_id", "historical_date"),
        all.x = TRUE)
    stopifnot("exnamhos" %in% names(df))
    
    return(df)
}

set_to_missing <- function(df) {

    info("Removing observations where exnamhos is missing")
    bool <- is.na(df[["exnamhos"]])
    stopifnot(`bool cannot have missingness` = !anyNA(bool))

    if (any(bool)) {
        df <- clean_observations(df, bool, "set_to_missing_exnamhos",
            "Observations set to missing where exnamhos is missing")
    }

    df[["exnamhos"]] <- NULL
    
    return(df)
}

main <- function(codebook, df, exnamhos, TASK_NAME) {

    check_date_specific_rule(codebook, TASK_NAME)
    df <-
        merge_exnamhos(df, exnamhos) |>  
        set_to_missing(df=_)

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
        exnamhos <- objs[["v2exnamhos"]][["v2exnamhos"]]
    } else {
        exnamhos <- objs[["v3exnamhos"]][["v3exnamhos"]]
    }

    # Run
    collectedInputs <- named_list(codebook, df, exnamhos, TASK_NAME)
    setNames(list(do.call(main, collectedInputs), DIRTYLIST), 
        c(TASK_NAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
    info("Done with hos appointment dates cleaning")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/downstream_cleaning/test_hos_appointment_dates.R")
}
