#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Collapse several categories into fewer categories.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
merge_qtable <- function(df, qtable) {
	
    inRow <- nrow(df)
    df <- merge(
        x = df,
        y = qtable[, c("question_id", "name", "recoded")],
        by = "question_id",
        all.x = TRUE)
    stopifnot(inRow == nrow(df))
    return(df)
}

# collapse categories into fewer categories (hardcoded)
collapse_categories <- function(df, qtable) {

	stopifnot(`code outside range` = df$code %in% c(0, 1, 2, 3, 4, NA_real_))
    # Subset only recoded
    df <- df[with(df, recoded), ]
    # The collapsing
    df[["code"]] <- ifelse(
        df[["code"]] == 0, 0, ifelse(
            df[["code"]] %in% c(1, 2), 1, ifelse(
                df[["code"]] %in% c(3, 4), 2, NA_real_)))

    df[["name"]] <- paste0(df[["name"]], "_rec")
    df[["question_id"]] <- 
        trans(df[["name"]], to = "question_id", ttable = qtable, by = "name")
    
    return(df)
}

main <- function(df, qtable) {

    out <- collapse_categories(
        merge_qtable(df, qtable), qtable)

}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    qtable <- load_qtable()
    df <- find_dep_files(TASK_ID, db)[[
		gsub("_rec", "", TASK_NAME, fixed = TRUE)
		]][[gsub("_rec", "", TASK_NAME, fixed = TRUE)]]

    # Run
    collectedInputs <- named_list(df, qtable)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(OUTFILE, dir_create = TRUE)
    info("Create recoded variable versions for " %^% TASK_NAME)
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_recoded.R") %>%
		check_test_file_output()
}
