#!/usr/bin/env Rscript

# ==========================================================================
# A-variable v2exaphogp gets cleaned by A-variable v2expathhg
# ==========================================================================

library(vutils)
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "expathhg_cleaning")


#
# Functions
# --------------------------------------------------------------------------
merge_expathhg <- function(df, v2expathhg, utable) {
    df %<>%
        full_join(select(v2expathhg, expathhg, country_id, historical_date),
            by = c("country_id", "historical_date")) %>%
        interpolate_vdem_col("expathhg", utable)
    df <- df[!is.na(df$code) | !is.na(df$text_answer), ]
    return(df)
}

filter_dirty <- function(df) {
  return(df %>% filter(expathhg %in% c(7, 8)))
}

filter_clean <- function(df, dirt) {
  df %>%
    filter(!id %in% dirt$id) %>%
    select(-expathhg) %>%
    return(.)
}

check_cleaning_rule <- function(codebook, VARNAME) {
    rule <- codebook %>%
        select(question_id, tag, cleaning) %>%
        filter(tag == VARNAME) %$% cleaning
    stopifnot(length(rule) == 1)
    stopifnot(!is.na(rule))
    stopifnot(rule != "")
    stopifnot(grepl("missing", rule))
    stopifnot(grepl("expathhg", rule))
    return(NULL)
}

main <- function(country_unit, df, VARNAME, expathhg, codebook) {
    check_cleaning_rule(codebook, VARNAME)
    expathhg %<>% select(country_id, historical_date, expathhg = code)
    df <- merge_expathhg(df, expathhg, country_unit)
    dirt <- filter_dirty(df)
    dirtylist[["expathhg_dirt"]] <<- dirt
    return(filter_clean(df, dirt))
}

# Run script
if (no_test()) {
    # Global variables
    get_globals()
    dirtylist <- list()

    # Imports
    codebook <-  load_codebook()
    country_unit <- load_country_unit()
    objs <- find_dep_files(TASK_ID, DB)
    df <- objs[[VARNAME]][[VARNAME]]
    expathhg <- objs[["v2expathhg"]][["v2expathhg"]]

    # Run
    collectedInputs <- named_list(country_unit, df, VARNAME, expathhg, codebook)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with expathhg_cleaning...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/downstream_cleaning_2/expathhg_cleaning.R")
}
update_task_status(db = DB)