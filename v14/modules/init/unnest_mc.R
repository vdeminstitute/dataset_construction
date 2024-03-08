#!/usr/bin/env Rscript

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
        x=df, y=qtable[, c("question_id", "name", "class", "question_type")],
        by="question_id", all.x=TRUE)
    stopifnot(inRow == nrow(df))

    return(df)
}

# get possible responses for a given question
get_responses <- function(qtable, TASK_NAME) {
    responses <- as.numeric(unlist(strsplit(qtable[["cb_responses"]], " ")))
    stopifnot(length(responses) > 0)
    
    return(responses)
}

# One hot coding for multiple categories
one_hot <- function(df, responses, TASK_NAME) {
    stopifnot(is.numeric(responses))

    df[["codeF"]] <- factor(df[["code"]], levels = responses)
    # na.action=na.pass is needed to avoid dropping rows with NA
    onehot <- as.data.frame(model.matrix(~ 0 + codeF,
        model.frame(~ codeF, data = df, na.action=na.pass)))
    colnames(onehot) <- sprintf("%s_%s", TASK_NAME, responses)

    stopifnot(nrow(df) == nrow(onehot))
    stopifnot(length(responses) == ncol(onehot))

    return(onehot)
}

# return augmented results as one hot encoded data.frames
# -- question_id is updated and name is removed
augment_one_hot <- function(oneHotData, df, qtable, newNames) {

    ohNames <- colnames(oneHotData)
    res <- lapply(ohNames, function(i, onehot, dat,  qta, nN) {
        
        dat[["name"]] <- i
        dat[["code"]] <- onehot[[i]]
        dat[["question_id"]] <- vutils::trans(
            v = i,
            to = "question_id",
            ttable = qta,
            by = "name")
        
        dat <- dat[, nN]
        vbase::organiseRows(dat, country_id, coder_id, historical_date)

        return(dat)
        }, onehot=oneHotData,dat=df,qta=qtable,nN=newNames)

    names(res) <- ohNames

    return(res)
}

main <- function(df, qtable, qtableTask, TASK_NAME) {

    dfNames <- names(df)
    df <- merge_qtable(df, qtableTask)
    responses <- get_responses(qtableTask, TASK_NAME)
    out <- augment_one_hot(one_hot(df, responses, TASK_NAME), df, qtable, dfNames)
    
    return(out)
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
    qtableTask <- qtable[qtable$name == TASK_NAME, ]
    df <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]
    stopifnot(nrow(qtableTask) == 1)

    # Run
    collectedInputs <- named_list(df, qtable, qtableTask, TASK_NAME)
    do.call(main, collectedInputs) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done with unnesting multiple categories for " %^% TASK_NAME)
} else {
    # Tests
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_unnest_mc.R") %>%
		check_test_file_output()
}
