#!/usr/bin/env Rscript

options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
suppressMessages(library(tidyr))
set_env(MODULE_NAME = "unnest_mc")

#
# Functions
# --------------------------------------------------------------------------
dichotomize_var <- function(df, qtable, VARNAME) {
    info("v2csstruc/v2casoe is not a multiple selection question, but we still " %^%
         "want to dichotomize it!")
    df %>% left_join_(qtable[, c("question_id", "name", "class", "question_type")],
                   by = "question_id") %>%
        filter(name == VARNAME) %>%
        return(.)
}

category_vector <- function(qtable, VARNAME) {
    cb_resp <- qtable$cb_responses[qtable$name == VARNAME] %>%
    strsplit(., " ") %>%
    unlist %>%
    as.numeric

    stopifnot(length(cb_resp) > 0)
    return(cb_resp)
}

prep_dich <- function(category, df_dichotomised, qtable) {
    stopifnot(is.numeric(category))
    lapply(category, function(category) {df_dichotomised %>%
                mutate(code = ifelse(code == category, 1, 0)) %>%
                arrange(desc(code)) %>%
                distinct(country_id, coder_id, question_id, historical_date, .keep_all = T) %>%
                mutate(name = name %^% "_" %^% category) %>%
                mutate(question_id = trans(v = name, to = "question_id",
                    ttable = qtable, by = "name"))}) %>%
    bind_rows(.) %>%
    return(.)
}

prep_out <- function(df, col_names) {
    df %<>% select(one_of(col_names), name)
    nms <- df$name
    df[["name"]] <- NULL
    return(split(df, as.factor(nms)))
}

main <- function(df, qtable, VARNAME, col_names) {
    df_dich <- dichotomize_var(df, qtable, VARNAME)
    cat_vec <- category_vector(qtable, VARNAME)
    prep_df <- prep_dich(cat_vec, df_dich, qtable)
    return(prep_out(prep_df, col_names))
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()
    
    # Imports
    qtable <- load_qtable()
    df <- find_dep_files(TASK_ID, DB)[[VARNAME]][[VARNAME]]
    col_names <- colnames(df)

    if (!VARNAME %in% c("v2csstruc", "v2casoe"))
        stop("Should this variable really go through this script? " %^% VARNAME)

    # Run
    collectedInputs <- named_list(df, qtable, VARNAME, col_names)
    do.call(main, collectedInputs) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done with unnesting multiple categories for " %^% VARNAME)
} else {
    # Tests
    testthat::test_file("~/proj/mm-prep/tests/init/test_unnest_mc.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)