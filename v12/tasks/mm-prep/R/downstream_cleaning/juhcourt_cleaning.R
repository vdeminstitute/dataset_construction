#!/usr/bin/env Rscript

# ==========================================================================
# cleaned by a-star v3juhcourt
# ==========================================================================

library(vutils)
suppressMessages(library(dplyr))
Sys.setenv(MODULE_NAME = "juhcourt_cleaning")


#
# Functions
# --------------------------------------------------------------------------


check_cleaning_rule <- function(codebook) {
    
    # get date specific rule for this variable
    rule <- 
        codebook %>%
        select(question_id, tag, cleaning) %>%
        filter(tag == VARNAME) %$% cleaning
    stopifnot(length(rule) == 1)
    stopifnot(!is.na(rule))
    stopifnot(rule != "")
    stopifnot(grepl("missing", rule))
    stopifnot(grepl("juhcourt", rule))
}

merge_juhcourt <- function(df, juhcourt_df, utable) {
    # get juhcourt == 1 data
    juhcourt_df %<>% select(juhcourt = code, country_id, historical_date)

    df %<>%
        full_join(juhcourt_df, by = c("country_id", "historical_date")) %>%
        interpolate_vdem_col("juhcourt", utable)
    df <- df[!is.na(df$code) | !is.na(df$text_answer), ]
    return(df)
}

filter_dirty <- function(df, varname) {
    dirt <- df %>% filter((juhcourt == 0) | is.na(juhcourt))

    info("cleaning " %^% nrow(dirt) %^% " observations for cleaning rule juhcourt is 0" %^%
        "variable " %^% varname)
    return(dirt)
}

filter_clean <- function(df, dirt) {
    df %>%
        filter(!id %in% dirt$id) %>%
        select(-juhcourt) %>%
        return(.)
}

main <- function(country_unit, codebook, df, juhcourt_df, VARNAME) {
    check_cleaning_rule(codebook)
    df <- merge_juhcourt(df, juhcourt_df, country_unit)
    dirt <- filter_dirty(df, VARNAME)
    dirtylist[["juhcourt_cleaning"]] <<- dirt
    return(filter_clean(df, dirt))
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
        juhcourt_df <- objs[["v2juhcourt"]][["v2juhcourt"]]
    } else {
        juhcourt_df <- objs[["v3juhcourt"]][["v3juhcourt"]]
    }
    stopifnot(!is.null(juhcourt_df))

    # Run
    collectedInputs <- named_list(country_unit, codebook, df, juhcourt_df, VARNAME)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)     
    info("Done with clean_all")
} else {
    testthat::test_file("~/proj/mm-prep/tests/init/test_juhcourt_cleaning.R")
}
update_task_status(db = DB)