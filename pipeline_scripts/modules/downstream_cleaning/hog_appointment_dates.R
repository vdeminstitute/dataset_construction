#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# cleaned by a-star v2exnamhog and v3exnamhog
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
    stopifnot(grepl("HOG appointment dates", fixed = TRUE, x = rule))
}

merge_exnamhog <- function(df, exnamhog) {
    
    stopifnot("code" %in% names(df))

    exnamhog <- exnamhog[, c("country_id", "historical_date", "text_answer")]
    names(exnamhog) <- c("country_id", "historical_date", "exnamhog")
    stopifnot(!"text_answer" %in% names(exnamhog))
    
    df <- merge(
        x = df,
        y = exnamhog,
        by = c("country_id", "historical_date"),
        all.x = TRUE)
    stopifnot("exnamhog" %in% names(df))

    return(df)
}

# Set the value of df to missing where exnamhog is missing
set_to_missing <- function(df) {
    
    bool <- is.na(df[["exnamhog"]])
    stopifnot(`bool cannot have missingness` = !anyNA(bool))

    if (any(bool)) {

        df <- clean_observations(df, bool, "set_to_missing_exnamhog",
            "Observations set to missing where exnamhog is missing")
    }

    df[["exnamhog"]] <- NULL
    
    return(df)
}

main <- function(codebook, df, exnamhog, TASK_NAME) {
    
    check_date_specific_rule(codebook, TASK_NAME)
    out <- 
        merge_exnamhog(df, exnamhog) |>  
		set_to_missing(df=_)

    return(out)
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
        exnamhog <- objs[["v2exnamhog"]][["v2exnamhog"]]
    } else {
        exnamhog <- objs[["v3exnamhog"]][["v3exnamhog"]]
    }

    # Run
    collectedInputs <- named_list(codebook, df, exnamhog, TASK_NAME)
    setNames(list(do.call(main, collectedInputs), DIRTYLIST), 
        c(TASK_NAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
    info("Done with hog appointment date cleaning")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/downstream_cleaning/test_hog_appointment_dates.R")
}
