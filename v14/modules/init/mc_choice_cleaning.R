#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# clean multiple-choice response data to only include valid responses.
# Valid data is found in cb_responses column of qtable.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
assert_multiple_choice <- function(qtable) {
    stopifnot(
        with(qtable, {
            question_type %in% c("M", "Y");
            !is.na(cb_responses);
            cb_responses != ""
        })
    )
}

# List responses as vector with NA 
list_responses <- function(qtable, TASK_NAME) {
    stopifnot(
        class(qtable) == "data.frame",
        nrow(qtable) == 1,
        grepl("\\d", qtable$cb_responses))
    
    info(sprintf("Responses for %s: %s", TASK_NAME, qtable$cb_responses))
    respVec <- as.numeric(c(unlist(strsplit(qtable$cb_responses, " ")[[1]])))
    
    return(respVec)
}

# Drop invalid responses
remove_invalid_mc_responses <- function(df, responses) {

    stopifnot(`df has zero rows` = nrow(df) > 0)

    bool <- !df[["code"]] %in% responses
    stopifnot(`bool cannot have missingness` = !anyNA(bool))

    if (any(bool)) {
        df <- clean_observations(df, bool, "remove_invalid_mc_responses",
        "Invalid MC responses")
    }

    return(df)
}

main <- function(qtable, df, TASK_NAME) {
    info("Cleaning outside coding range for multiple-choice)")
    
    assert_multiple_choice(qtable)
    df <- remove_invalid_mc_responses(
        df=df, responses=list_responses(qtable, TASK_NAME))
    
    return(df)
}

# Run script
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    qtable <- load_qtable()
    qtable <- qtable[qtable$name == TASK_NAME, ]
    objs <- find_dep_files(TASK_ID, db)
    df <- objs[[TASK_NAME]][[TASK_NAME]]
    stopifnot(nrow(qtable) == 1)

    # Run
    collectedInputs <- named_list(qtable, df, TASK_NAME)
    setNames(list(do.call(main, collectedInputs), DIRTYLIST), 
        c(TASK_NAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
    info("Done with mc_choice_cleaning")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_mc_choice_cleaning.R") %>%
		check_test_file_output()
}
