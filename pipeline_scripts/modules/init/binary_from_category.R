#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Create binary variables from baseline categories
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
# Merge in qtable
merge_qtable <- function(df, qtable) {

    inRow <- nrow(df)
    df <- merge(
        x = df,
        y = qtable[, c("question_id", "name", "recode_category_to_binary")],
        by = "question_id",
        all.x = TRUE)
    stopifnot(inRow == nrow(df))
    stopifnot(!all(is.na(df[["recode_category_to_binary"]])))

    return(df)
}

# turn baseline to 0 all others 1 and transform question_id
binary_version <- function(df, qtable) {

    df[["code"]] <-
        ifelse(
            test = df[["code"]] == df[["recode_category_to_binary"]],
            yes = 0, no = 1)
    df[["name"]] <- paste0(df[["name"]], "bin")
    df[["question_id"]] <-
        trans(df[["name"]], to = "question_id", ttable = qtable, by = "name")

    stopifnot(all(df[["code"]] %in% c(0, 1, NA_real_)))
    return(df)
}

# transform original version: remove the baseline category
original_cleanup <- function(df) {

    recCat <- unique(df[["recode_category_to_binary"]])
    codeTable <- table(df[["code"]], useNA = "always")
    expectedToBeRecoded <- codeTable[names(codeTable)==recCat & !is.na(names(codeTable))]
    
    # For these variables we need to adjust the categories by -1
    originals <- c("v2mecenefi", "v2lgdsadlo")

    # Drop ratings where code == recode_category_to_binary
    dropID <- df[["id"]][with(df, code == recode_category_to_binary & !is.na(code))]
    stopifnot(length(dropID) == expectedToBeRecoded)
    df <- df[!df[["id"]] %in% dropID, ]
    info(sprintf("Dropping %s observations following binary recoding",
        length(dropID)))

    # Shift by -1 for originals
    df[["code"]] <- ifelse(df[["name"]] %in% originals,
        df[["code"]] - 1, df[["code"]])
    
    # Code can be missing, but ID values should not be missing
    with(df, {
        stopifnot(!anyNA(country_id))
        stopifnot(!anyNA(question_id))
        stopifnot(!anyNA(coder_id))
        stopifnot(!anyNA(historical_date))
    })

    inNA <- codeTable[is.na(names(codeTable))]
    stopifnot(inNA == sum(is.na(df[["code"]])))
    df[["recode_category_to_binary"]] <- NULL
    return(df)
}

main <- function(qtable, df) {
    
    df <- merge_qtable(df, qtable)
    out <- list(original_cleanup(df), binary_version(df, qtable))
    return(out)

}

# Run script
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    qtable <-  load_qtable()
    objs <- find_dep_files(TASK_ID, db)
    df <- objs[[TASK_NAME]][[TASK_NAME]]

    # Run
    collectedInputs <- named_list(qtable, df)
    setNames(do.call(main, collectedInputs), 
        c(TASK_NAME, TASK_NAME %^% "bin")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with binary_from_category...")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_binary_from_category.R") %>%
		check_test_file_output()
}
