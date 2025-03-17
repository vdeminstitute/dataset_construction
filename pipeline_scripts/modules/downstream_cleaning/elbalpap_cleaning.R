#!/usr/bin/env Rscript

# ==========================================================================
# C-variable v3elbalstat gets cleaned by C-variable v3elbalpap per coder!
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------

# Check cleaning rule
check_cleaning_rule <- function(codebook, cleaningVar) {
    cleaning <- codebook[["cleaning"]]
    stopifnot(`rule from codebook is of length zero` = length(cleaning) > 0)
    
    ruleVec <- unlist(strsplit(x = cleaning, split = ";"))
    rule <- ruleVec[grepl(cleaningVar, ruleVec)]
    stopifnot(`after splitting the rule, found some delimiters` =
        !grepl(pattern = "[;:/]", x = rule))

    stopifnot(`no cleaning rule could be found` = length(rule) == 1)
    stopifnot(`we only set values to missing`= grepl("missing", rule))
    stopifnot(`we only remove based on numeric values for cleaning_var` = 
        grepl("\\d\\.?$", rule))
    stopifnot(!is.na(rule))
    stopifnot(rule != "")
	stopifnot(grepl(cleaningVar, rule, fixed = TRUE))

    return(rule)
}

# Merge in cleaningVarDf
merge_elbalpap <- function(df, cleaningVarDf, utable) {

    stopifnot(`elbalbap_df is not a data.frame` = is.data.frame(cleaningVarDf))
    stopifnot(`df is not a data.frame` = is.data.frame(df))

    cleaningVarDf[["cleaningVar"]] <- cleaningVarDf[["code"]]
    cleaningVarDf <- cleaningVarDf[, c("cleaningVar", "country_id", "historical_date", "coder_id")]

    stopifnot("code" %in% names(df))
    stopifnot(!"code" %in% names(cleaningVarDf))

    df <-
        merge(x = df, y = cleaningVarDf, 
            by = c("country_id", "historical_date", "coder_id"), 
            all= TRUE) |>
        vutils::interpolate_components(
            df=_, cols = "cleaningVar", utable,
            keep_nan = FALSE, coder_level = TRUE) |>
        organiseRows(df=_, country_id, historical_date)
    df <- df[with(df, !is.na(code) | !is.na(text_answer)), , drop = FALSE]
    stopifnot("cleaningVar" %in% names(df))

    return(df)
}

set_to_missing_elbalpap <- function(df) {

    # if elbalpap = 0 only, set to missing
    bool <- df[["cleaningVar"]] == 0 & !is.na(df[["cleaningVar"]])
    stopifnot(`bool cannot have missingness` = !anyNA(bool))

    if (any(bool)) {
        df <- clean_observations(df, bool, "set_to_missing_elbalpap",
            "Observations set to missing based on elbalpap = 0")
    }

    df[["cleaningVar"]] <- NULL
    return(df)
}

main <- function(utable, codebook, df, cleaningVarDf, cleaningVar, TASK_NAME) {
    
    # Assert and retrieve cleaning rule
    rule <- check_cleaning_rule(codebook, cleaningVar)
    info(sprintf("Running %s_cleaning for %s with rule: [%s]",
        cleaningVar, TASK_NAME, rule))
    df <-
        merge_elbalpap(df, cleaningVarDf, utable) |> 
        set_to_missing_elbalpap(df=_) |> 
        vbase::organiseRows(df=_, country_id, historical_date)

    return(df)
}

# Run script
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    utable <- load_country_unit()
    codebook <- load_codebook()
    qtable <- load_qtable()
    codebook <- codebook[codebook$tag == TASK_NAME, ]
    qtable <- qtable[qtable$name == TASK_NAME, ]
    objs <- find_dep_files(TASK_ID, db)
    df <- objs[[TASK_NAME]][[TASK_NAME]]
    cleaningVar <- ifelse(grepl("^v2", TASK_NAME), "v2elbalpap", "v3elbalpap")

    # This exists, but we currently only clean for v3elbalstat
    if (grepl("^v2", cleaningVar)) {
        cleaningVarDf <- objs[["v2elbalpap"]][["v2elbalpap"]]
    } else {
        cleaningVarDf <- objs[["v3elbalpap"]][["v3elbalpap"]]
    }

    # Run
    collectedInputs <- named_list(utable, codebook, df, cleaningVarDf, cleaningVar, TASK_NAME)
    setNames(list(do.call(main, collectedInputs), DIRTYLIST), 
        c(TASK_NAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
    info("Done with elbalpap_cleaning")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/downstream_cleaning/test_elbalpap_cleaning.R")
}
