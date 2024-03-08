#!/usr/bin/env Rscript

# cleaned by a-star v2exhoshog and v3exhoshog

library(vutils)
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "exhoshog_cleaning")

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
    stopifnot(grepl("exhoshog", rule))
}

merge_exhoshog <- function(df, exhoshog_df, utable) {
    # get exhoshog == 1 data
    exhoshog_df %<>% select(exhoshog = code, country_id, historical_date)
    stopifnot("code" %in% names(df))

    df %<>% full_join(exhoshog_df, by = c("country_id", "historical_date")) %>%
        interpolate_vdem_col("exhoshog", utable)
    df <- df[!is.na(df$code) | !is.na(df$text_answer), ]
    return(df)
}

filter_dirty <- function(df, VARNAME) {
    df %>% filter(exhoshog == 1 | is.na(exhoshog)) %T>%
        {info("cleaning " %^% nrow(.) %^% 
            " observations for cleaning rule exhoshog is 1" %^%
            "variable " %^% VARNAME)} %>%
        return(.)
}

filter_clean <- function(df, dirt) {
    df %>%
        filter(!id %in% dirt$id) %>%
        select(-exhoshog) %>%
        return(.)
}

main <- function(country_unit, codebook, df, exhoshog_df) {
    check_cleaning_rule(codebook, VARNAME)
    df <- merge_exhoshog(df, exhoshog_df, country_unit)
    dirt <- filter_dirty(df, VARNAME)
    dirtylist[["exhoshog_cleaning"]] <<- dirt
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
        exhoshog_df <- objs[["v2exhoshog"]][["v2exhoshog"]]
    } else {
        exhoshog_df <- objs[["v3exhoshog"]][["v3exhoshog"]]
    }
    
    # Run
    collectedInputs <- named_list(country_unit, codebook, df, exhoshog_df)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with exhoshog_cleaning...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/downstream_cleaning/test_exhoshog_cleaning.R")
}
update_task_status(db = DB)