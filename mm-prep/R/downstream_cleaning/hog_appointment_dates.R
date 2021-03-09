#!/usr/bin/env Rscript

# ==========================================================================
# cleaned by a-star v2exnamhog and v3exnamhog 
# ==========================================================================
library(vutils)
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "hog_appointment_dates")


#
# Functions
# --------------------------------------------------------------------------
merge_exnamhog <- function(df, exnamhog_df) {
    exnamhog_df %<>%
        select(exnamhog = text_answer, country_id, historical_date)

    df %<>%
        left_join(exnamhog_df, by = c("country_id", "historical_date"))
}

filter_dirty <- function(df, VARNAME) {
    dirt <- df %>% filter(is.na(exnamhog))

    info("cleaning " %^% nrow(dirt) %^% " observations for date specific")
    return(dirt)
}

filter_clean <- function(df, dirt) {
    df %<>% filter(!id %in% dirt$id) %>% select(-exnamhog)
}

main <- function(codebook, df, exnamhog_df, VARNAME) {
    codebook %>%
        select(question_id, tag, date_specific) %>%
        filter(tag == VARNAME) %$% date_specific %>%
        {stopifnot(grepl("HOG appointment dates", .))}
    df <- merge_exnamhog(df, exnamhog_df)
    dirt <- filter_dirty(df, VARNAME)
    dirtylist[["hog_date_cleaning"]] <<- dirt
    return(filter_clean(df, dirt))
}

# Run script
if (no_test()) {
    # Global variables
    get_globals()
    dirtylist <- list()

    # Imports
    codebook <- load_codebook()
    objs <- find_dep_files(TASK_ID, DB)
    df <- objs[[VARNAME]][[VARNAME]]
    if (grepl("^v2", VARNAME)) {
        exnamhog_df <- objs[["v2exnamhog"]][["v2exnamhog"]]
    } else {
        exnamhog_df <- objs[["v3exnamhog"]][["v3exnamhog"]]
    }

    # Run
    collectedInputs <- named_list(codebook, df, exnamhog_df, VARNAME)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with hog appointment date cleaning...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/downstream_cleaning/test_hog_appointment_dates.R")
}
update_task_status(db = DB)