#!/usr/bin/env Rscript

# ==========================================================================
# C-variable v3elbalstat gets cleaned by C-variable v3elbalpap per coder!
# ==========================================================================

library(vutils)
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "elbalpap_cleaning")


#
# Functions
# --------------------------------------------------------------------------

check_cleaning_rule <- function(codebook, VARNAME) {
    # get date specific rule for this variable
    rule <-
        codebook %>%
        select(question_id, tag, cleaning) %>%
        filter(tag == VARNAME) %$% cleaning
    stopifnot(length(rule) == 1)
    stopifnot(!is.na(rule))
    stopifnot(rule != "")
    stopifnot(grepl("missing", rule))
    stopifnot(grepl("elbalpap", rule))
}


merge_and_clean <- function(df, elbalpap_df, utable) {
    elbalpap_df %<>%
        select(elbalpap = code, country_id, historical_date, coder_id)
    df %<>%
        full_join(elbalpap_df, by = c("country_id", "historical_date", "coder_id")) %>%
        interpolate_vdem_col("elbalpap", utable)
    df <- df[!is.na(df$code) | !is.na(df$text_answer), ]
    return(df)
}

filter_dirty <- function(df) {
    dirt <- df %>% filter((elbalpap == 0) | is.na(elbalpap))
    info("cleaning " %^% nrow(dirt) %^% " observations for cleaning rule elbalpap is 0")
    return(dirt)
}

clean_df <- function(df, dirt) {
    df %>%
        filter(!id %in% dirt$id) %>%
        select(-elbalpap)
}

main <- function(country_unit, codebook, df, elbalpap_df, VARNAME) {
    check_cleaning_rule(codebook, VARNAME)
    df <- merge_and_clean(df, elbalpap_df, country_unit)
    dirty_df <- filter_dirty(df)
    dirtylist[["elbalpap_cleaning"]] <<- dirty_df
    return(clean_df(df, dirty_df))
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
        elbalpap_df <- objs[["v2elbalpap"]][["v2elbalpap"]]
    } else {
        elbalpap_df <- objs[["v3elbalpap"]][["v3elbalpap"]]
    }

    # Run
    collectedInputs <- named_list(country_unit, codebook, df, elbalpap_df, VARNAME)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with elbalpap_cleaning...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/downstream_cleaning/test_elbalpap_cleaning.R")
}
update_task_status(db = DB)