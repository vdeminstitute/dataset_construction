#!/usr/bin/env Rscript

options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
suppressMessages(library(tidyr))
set_env(MODULE_NAME = "unnest_ms")

#
# Functions
# --------------------------------------------------------------------------
append_qtable <- function(df, qtable) {
    df <- left_join_(df, qtable[, c("question_id", "name", "class", "question_type")],
                   by = "question_id")
    stopifnot(df$question_type == "S")
    return(df)
}

calc_choices <- function(df, qtable) {
    info("Here possible choices are derived from data, this is dangerous!")
    info("We will clean those out before!")
    choices <- df %>%
        mutate(ms_choices = strsplit(text_answer, ",") %>%
                            unlist %>%
                            na.omit %>%
                            unique %>%
                            as.numeric %>%
                            sort %>%
                            paste(collapse = ",")) %>%
        select(name, ms_choices) %>%
        distinct %>%
        as.data.frame(stringsAsFactors = FALSE)

    choices <-  left_join_(choices, qtable[, c("name", "cb_responses", "choice_values")],
                        by = "name")
    return(choices)
}

calc_ms_expanded <- function(df, choices_df) {
    ms_frame <- df %>%
        filter(question_type == "S") %>% 
        left_join_(choices_df, by = "name") %>%
        mutate(text_answer = strsplit(cb_responses, " ")) %>%
        unnest(text_answer) %>%
        filter(!is.na(text_answer)) %>%
        distinct %>%
        mutate(name = name %^% "_" %^% trimws(text_answer)) %>%
        select(-code)

    info("Expand multiple-selection variables...")
    ms.df <- df %>%
        filter(question_type == "S") %>%
        mutate(text_answer = strsplit(text_answer, ",")) %>%
        unnest(text_answer) %>%
        filter(!is.na(text_answer)) %>%
        distinct %>%
        mutate(code = 1,
               name = name %^% "_" %^% trimws(text_answer))
    ms_expanded <- left_join(ms_frame, ms.df)
    ms_expanded$code[is.na(ms_expanded$code)] <- 0
    is.na(ms_expanded$text_answer) <- TRUE
    return(ms_expanded)
}

dirty_names <- function(ms_expanded) {
    info("Filter away observations that are outside the answer categories...")
    dirty_names <- ms_expanded %>%
        group_by(name) %>%
        filter(!as.numeric(gsub(".*?_(\\d+)$", "\\1", name)) %in%
                 as.numeric(unlist(strsplit(first(cb_responses), " ")))) %>%
        ungroup %$%
        unique(name)
    return(dirty_names)
}

remove_dirty <- function(df, dirt_names, qtable) {
    df <- filter(df, !name %in% dirt_names)
    info("Are all new variables in our question table?")
    stopifnot(unique(df$name) %in% qtable$name)
    return(df)
}

append_ms_expanded <- function(df, ms_expanded, qtable) {
    info("We still need to transform to the new question_ids for each binary variable..")
    ms_expanded %<>% select(-question_id) %>%
        left_join_(qtable[, c("name", "question_id")], by = "name")
    stopifnot(!anyNA(ms_expanded$question_id))
    
    info("Merge back together mutliple-selection with other data...")
    df %<>% 
        filter(question_type != "S") %>%
        bind_rows(ms_expanded)
    stopifnot(!anyNA(df$question_id))
    return(df)
}

main <- function(df, qtable) {
    n <- colnames(df)
    df %<>% append_qtable(., qtable)
    choices <- calc_choices(df, qtable)
    # it takes a while
    ms_expanded <- calc_ms_expanded(df, choices) %>%
        as.data.frame()
    dirt_names <- dirty_names(ms_expanded)

    dirt <- ms_expanded %>%
        filter(name %in% dirt_names) %>%
        filter(code == 1)
    ms_expanded <- remove_dirty(ms_expanded, dirt_names,
        qtable)
    df_out <- append_ms_expanded(df, ms_expanded,
        qtable)

    df_out %<>% select(one_of(n), name)
    nms <- df_out$name
    df_out %<>% select(-name)
    out <- c(split(df_out, as.factor(nms)), list(dirty = dirt))
    return(out)
}

#
# Run functions
# --------------------------------------------------------------------------
if(no_test()) {
    # Global variables
    get_globals()

    # Imports
    qtable <- load_qtable()
    df <- find_dep_files(TASK_ID, DB)[[VARNAME]][[VARNAME]]

    # Run
    collectedInputs <- named_list(df, qtable)
    do.call(main, collectedInputs) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done with unnesting multiple selection vars")
} else {
    # Tests
    testthat::test_file("~/proj/mm-prep/tests/init/test_unnest_ms.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)