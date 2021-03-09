#!/usr/bin/env Rscript

# ==========================================================================
# cleaned by a-star v2exnamhos and v3exnamhos
# ==========================================================================

library(vutils)
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "hos_appointment_dates")

#
# Functions
# --------------------------------------------------------------------------
merge_exnamhos <- function(df, exnamhos_df) {
    exnamhos_df %<>%
        select(exnamhos = text_answer, country_id, historical_date)
    df %>%
        left_join(exnamhos_df, by = c("country_id", "historical_date")) %>%
        return(.)
}

filter_dirty <- function(df, varname) {
    dirt <- df %>% filter(is.na(exnamhos))
    info("cleaning " %^% nrow(dirt) %^% " observations for date specific")
    return(dirt)
}

filter_clean <- function(df, dirt) {
   df %>% filter(!id %in% dirt$id) %>% select(-exnamhos) %>% return(.)
}

main <- function(codebook, df, exnamhos_df, VARNAME) {
    codebook %>%
        select(question_id, tag, date_specific) %>%
        filter(tag == VARNAME) %$% date_specific %>%
        {stopifnot(grepl("HOS appointment dates", .))}
    df <- merge_exnamhos(df, exnamhos_df)
    dirt <- filter_dirty(df, VARNAME)
    dirtylist[["hos_cleaning"]] <<- dirt
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
        exnamhos_df <- objs[["v2exnamhos"]][["v2exnamhos"]]
    } else {
        exnamhos_df <- objs[["v3exnamhos"]][["v3exnamhos"]]
    }

    # Run
    collectedInputs <- named_list(codebook, df, exnamhos_df, VARNAME)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with hos appointment dates cleaning...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/downstream_cleaning/test_hos_appointment_dates.R")
}
update_task_status(db = DB)