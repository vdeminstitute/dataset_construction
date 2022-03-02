#!/usr/bin/env Rscript

options(warn = 2)
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "mc_choice_cleaning")


#
# Functions
# --------------------------------------------------------------------------
list_responses <- function(qtable) {
    stopifnot(class(qtable) == "data.frame", nrow(qtable) == 1,
        grepl("\\d", qtable$cb_responses))
    resp <- first(qtable$cb_responses)
    info(": codebook responses: " %^% qtable$cb_responses)
    return(resp)
}

filter_dirty <- function(df, responses) {
    dirty_id <- df %>%
        filter(!code %in% as.numeric(unlist(strsplit(responses, " ")))) %>%
        pull(id)
    info("Number of codebook responses cleaned: " %^% length(dirty_id))

    outread <- filter(df, id %in% dirty_id) %>%
        group_by(code) %>%
        summarize(n = n()) %>%
        arrange(desc(n)) %>%
        as.data.frame(stringsAsFactors = FALSE)

    if (nrow(outread) > 0) {
        info(" Observations cleaned by mc-choice cleaning_id:010  " %^%
            outread$code %^% ":" %^% outread$n)
    }
    return(dirty_id)
}

main <- function(qtable, df, VARNAME) {
    info("Clean outside coding range... (only for multiple-choice)")
    qtable %<>% filter(name == VARNAME)
    stopifnot({qtable$question_type %in% c("M", "Y")})
    stopifnot({!is.na(qtable$cb_responses)})
    stopifnot({qtable$cb_responses != ""})

    dirty_id <- filter_dirty(df, list_responses(qtable))

    dirtylist[["bad_mc_choices"]] <<- df %>% filter(id %in% dirty_id)
    df %>%
        filter(!id %in% dirty_id) %>%
        return(.)
}

# Run script
if (no_test()) {
    # Global variables
    get_globals()
    dirtylist <- list()

    # Imports
    qtable <- load_qtable()
    objs <- find_dep_files(TASK_ID, DB)
    df <- objs[[VARNAME]][[VARNAME]]

    # Run
    collectedInputs <- named_list(qtable, df, VARNAME)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with mc_choice_cleaning...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/init/test_mc_choice_cleaning.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)