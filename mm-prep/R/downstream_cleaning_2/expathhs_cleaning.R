#!/usr/bin/env Rscript

# ==========================================================================
# A-variable v2exaphos gets cleaned by A-variable v2expathhs
# ==========================================================================
library(vutils)
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "expathhs_cleaning")


#
# Functions
# --------------------------------------------------------------------------
merge_expathhs <- function(df, v2expathhs, utable) {
    df %<>%
        full_join(select(v2expathhs, expathhs, country_id, historical_date),
            by = c("country_id", "historical_date")) %>%
        interpolate_vdem_col("expathhs", utable)
    df <- df[!is.na(df$code) | !is.na(df$text_answer), ]
    return(df)
}

filter_dirty <- function(df) {
  return(df %>% filter(expathhs %in% c(6, 7)))
}

filter_clean <- function(df, dirt) {
  df %>%
    filter(!id %in% dirt$id) %>%
    select(-expathhs) %>%
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
    stopifnot(grepl("expathhs", rule))
    return(NULL)
}

main <- function(country_unit, df, VARNAME, expathhs, codebook) {
    check_cleaning_rule(codebook, VARNAME)
    expathhs %<>% select(country_id, historical_date, expathhs = code)
    df <- merge_expathhs(df, expathhs, country_unit)
    dirt <-  filter_dirty(df)
    dirtylist[["expathhs_dirt"]] <<- dirt
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
    expathhs <- objs[["v2expathhs"]][["v2expathhs"]]

    # Run
    collectedInputs <- named_list(country_unit, df, VARNAME, expathhs, codebook)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with expathhs_cleaning...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/downstream_cleaning_2/expathhs_cleaning.R")
}
update_task_status(db = DB)