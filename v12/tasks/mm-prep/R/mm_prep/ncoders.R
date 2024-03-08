#!/usr/bin/env Rscript

# ==========================================================================
# ./ncoders.R prep/raw prep/ncoders.rds
# Calculate the number of coders per variable-country-year using the
# raw split rds files. This is reported in the final DS as v2*_nr.
# ==========================================================================

options(warn = 2)

suppressMessages(library(docopt))
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(vutils))
suppressMessages(library(parallel))
set_env(MODULE_NAME = "ncoders")


#
# Functions
# --------------------------------------------------------------------------
fix_varnames <- function(df, VARNAME) {
    if (!grepl("_\\d+$", VARNAME)) {
        colnames(df)[colnames(df) == "ncoders"] <- VARNAME %^% "_nr"
    } else {
        colnames(df)[colnames(df) == "ncoders"] <-
            gsub("_\\d+$", "", VARNAME) %^% "_nr"
    }
    return(df)
}

country_date_nr <- function(m) {
    res <- rowSums(!is.na(m))
    df <- data.frame(country_text_id = get_text_id(names(res)), 
                     historical_date = get_date(names(res)),
                     ncoders = res) %>%
        arrange(country_text_id, historical_date) %>%
        mutate(historical_date = as.Date(historical_date))
    rownames(df) <- NULL
    return(df)
}

country_year_nr <- function(m) {
    m <- !is.na(m)
    m_split <- split.data.frame(m, 
        list(get_text_id(rownames(m)), 
             substr(rownames(m), 5, 8)), drop = TRUE)
    
    res <- lapply(m_split, function(lll) {
        # lll <- m_split[["BOL.2019"]]
        if (nrow(lll) == 1)
            return(rowSums(lll))
        tempres <- matrix(colSums(lll), nrow = 1)
        tempres <- rowSums(tempres > 0)
        return(tempres)
    })

    
    df <- data.frame(country_text_id = substr(names(res), 1, 3),
                     year = as.numeric(substr(names(res), 5, 8)),
                     ncoders = unlist(res)) %>%
        arrange(country_text_id, year)
    rownames(df) <- NULL
    return(df)
}


main <- function(m, VARNAME) {
    cd <- country_date_nr(m) %>%
        fix_varnames(., VARNAME)

    cy <- country_year_nr(m) %>%
        fix_varnames(., VARNAME)

    return(list(cd = cd, cy = cy))
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    qtable <- load_qtable() %>% filter(name == VARNAME)
    objs <- find_dep_files(TASK_ID, DB)

    if (qtable$to_dichotomize) {
        m <- objs[[1]][[VARNAME %^% "_0"]]$wdata
    } else {
        m <- objs[[1]][[VARNAME]]$wdata
    }
    stopifnot(nrow(m) > 0)

    # Run
    collectedInputs <- named_list(m, VARNAME)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done with calculating the number of coders for " %^% VARNAME)

} else {
    # Call unit tests for main function and sub functions
    testthat::test_file("~/proj/mm-prep/tests/mm_prep/test_ncoders.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)