#!/usr/bin/env Rscript

# ==========================================================================
# cleaned by a-star v2eltype and v3eltype
# ==========================================================================

suppressMessages(library(vutils))
suppressMessages(library(dplyr))
Sys.setenv(MODULE_NAME = "eltype_dates")




#
# Functions
# --------------------------------------------------------------------------

check_cleaning_rule <- function(codebook, VARNAME) {
    rule <- codebook %>%
        select(question_id, tag, date_specific) %>%
        filter(tag == VARNAME) %$% date_specific
    stopifnot(length(rule) == 1)
    stopifnot(!is.na(rule))
    stopifnot(rule != "")
    stopifnot(grepl("Election|election", rule))
    return(rule)
}

merge_eltype <- function(df, eltype_df) {
    eltype_df %<>% select(country_id, historical_date, eltype = text_answer)
    df %>% 
        left_join(eltype_df, by = c("country_id", "historical_date")) %>%
        return(.)
}

filter_bad_el_dates <- function(df) {
    # all bad election dates
    df %>% filter(is.na(eltype)) %>%
        return(.)
}

more_dirty_data <- function(df, dirt, varname, rule) {
    more_dirt <- data.frame()

    if (grepl("eltype_0", rule, fixed = T)) {
        more_dirt <-
            df %>%
            filter(!is.na(eltype)) %>%
            filter(!grepl("0|1", eltype))
    }

    if (grepl("eltype_6", rule, fixed = T)) {
        more_dirt <-
            df %>%
            filter(!is.na(eltype)) %>%
            filter(!grepl("6|7", eltype))
    }

    if (grepl("eltype_2", rule, fixed = T)) {
        more_dirt <-
            df %>%
            filter(!is.na(eltype)) %>%
            filter(!grepl("2|3", eltype))
    }

    dirt <- bind_rows(dirt, more_dirt)
    info("cleaning " %^% nrow(dirt) %^% " observations for date specific")
    return(dirt)
}

final_cleaning <- function(df, dirt) {
    df %>%
        filter(!id %in% dirt$id) %>%
        select(-eltype) %>%
        return(.)
}

main <- function(country_unit, codebook, df, eltype_df, VARNAME) {
    rule <- check_cleaning_rule(codebook, VARNAME)
    df <- merge_eltype(df, eltype_df)
    dirt <- filter_bad_el_dates(df)
    dirt <- more_dirty_data(df, dirt, VARNAME, rule)
    dirtylist[["eltype_bad_dates"]] <<- dirt
    return(final_cleaning(df, dirt))
}



# Run script
if (no_test()) {
    # Global variables
    get_globals()
    dirtylist <- list()

    # Imports
    country_unit <- load_country_unit()
    codebook <- load_codebook()
    objs <- find_dep_files(TASK_ID, DB)
    df <- objs[[VARNAME]][[VARNAME]]
    if (grepl("^v2", VARNAME)) {
        eltype_df <- objs[["v2eltype"]][["v2eltype"]]
    } else {
        eltype_df <- objs[["v3eltype"]][["v3eltype"]]
    }

    # Run
    collectedInputs <- named_list(country_unit, codebook, df, eltype_df, VARNAME)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with eltype_dates...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/downstream_cleaning/test_eltype_dates.R")
}
update_task_status(db = DB)