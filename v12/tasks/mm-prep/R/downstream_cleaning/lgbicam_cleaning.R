#!/usr/bin/env Rscript

# ==========================================================================
# cleaned by a-star v2lgbicam and v3lgbicam
# ==========================================================================

library(vutils)
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "lgbicam_cleaning")


#
# Functions
# --------------------------------------------------------------------------
merge_lgbicam <- function(df, lgbicam_df, utable) {
    df %<>%
        full_join(select(lgbicam_df, lgbicam = code, country_id, historical_date),
              by = c("country_id", "historical_date")) %>%
        interpolate_vdem_col("lgbicam", utable)
    df <- df[!is.na(df$code) | !is.na(df$text_answer), ]
    return(df)
}

filter_dirty <- function(df, rule, VARNAME) {
    if (grepl("0 or 1", rule, fixed = T)) {
        dirt <- df %>% filter((lgbicam %in% c(0, 1)) | is.na(lgbicam))
        info("cleaning " %^% nrow(dirt) %^% " observations for cleaning rule lgbicam is 0 or 1")
    }

    if (grepl("0$", rule)) {
        dirt <- df %>% filter((lgbicam == 0) | is.na(lgbicam))
        info("cleaning " %^% nrow(dirt) %^% " observations for cleaning rule lgbicam is 0")
    }
    return(dirt)
}

filter_clean <- function(df, dirt) {
    df %>%
        filter(!id %in% dirt$id) %>%
        select(-lgbicam) %>%
        return(.)
}


main <- function(country_unit, codebook, df, lgbicam_df, VARNAME) {
    rule <- check_cleaning_rule(codebook, VARNAME)
    df  <- merge_lgbicam(df, lgbicam_df, country_unit)
    dirt <- filter_dirty(df, rule, VARNAME)
    dirtylist[["lgbicam_cleaning"]] <<- dirt
    return(filter_clean(df, dirt))
}


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
    stopifnot(grepl("lgbicam", rule))
    return(rule)
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
        lgbicam_df <- objs[["v2lgbicam"]][["v2lgbicam"]]
    } else {
        lgbicam_df <- objs[["v3lgbicam"]][["v3lgbicam"]]
    }

    # Run
    collectedInputs <- named_list(country_unit, codebook, df, lgbicam_df, VARNAME)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with lgbicam_cleaning...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/downstream_cleaning/test_lgbicam_cleaning.R")
}
update_task_status(db = DB)