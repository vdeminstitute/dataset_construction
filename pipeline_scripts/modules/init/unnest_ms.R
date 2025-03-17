#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Unnesting multiple selection variables into dichotomous variables.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
# Merge in qtable
merge_qtable <- function(df, qtableTask) {

    inRow <- nrow(df)
    df <- merge(
        x=df, y=qtableTask[, c("question_id", "name", "class", "question_type")],
        by="question_id", all.x=TRUE)
    stopifnot(inRow == nrow(df))
    stopifnot(df[["question_type"]] == "S")

    return(df)
}

define_choices <- function(df, qtableTask) {

    info("MS-choices will be derived from the data")
    msChoices <- with(df, 
        paste(sort(as.numeric(unique(unlist(na.omit(strsplit(text_answer, ",")))))),
            collapse=","))
    choices <- data.frame(name = unique(df[["name"]]), ms_choices = msChoices,
        stringsAsFactors = FALSE)

    stopifnot(is.data.frame(choices) & nrow(choices) == 1)

    res <- merge(
        x=choices, y=qtableTask[, c("name", "cb_responses", "choice_values")],
        by="name", all.x=TRUE)
    stopifnot(nrow(res) == nrow(choices))

    return(res)
}

expand_ms <- function(df, choices) {

    stopifnot(all(df[["question_type"]] == "S"))
    ms_frame <-
        merge(x=df, y=choices, by="name", all.x=TRUE) |> 
        mutate(text_answer = strsplit(cb_responses, " ")) |> 
        tidyr::unnest(text_answer) |> 
        filter(!is.na(text_answer)) |> 
        distinct() |> 
        mutate(name = name %^% "_" %^% trimws(text_answer)) |> 
        select(-code)

    info("Expand multiple-selection categories")
    ms_df <-
        filter(df, question_type == "S") |> 
        mutate(text_answer = strsplit(text_answer, ",")) |> 
        tidyr::unnest(text_answer) |> 
        filter(!is.na(text_answer)) |> 
        distinct() |> 
        mutate(code = 1, name = name %^% "_" %^% trimws(text_answer))

    ms_expanded <- merge(ms_frame, ms_df, all.x = TRUE)
    ms_expanded$code[is.na(ms_expanded$code)] <- 0
    is.na(ms_expanded$text_answer) <- TRUE
    ms_expanded <- as.data.frame(ms_expanded)

    return(ms_expanded)
}

cleanup_df <- function(df, ms_expanded, qtable) {

    info("Remove observations that are outside the answer categories")
    cb_resp <- ms_expanded[["cb_responses"]] |> 
        unique() |> 
        strsplit(x=_, split = " ") |>  
        unlist()

    ms_name <- ms_expanded[["name"]] |> 
        gsub(pattern = ".*?_(\\d+)$", replacement = "\\1", x = _)

    bool <- !(ms_name %in% cb_resp)
    stopifnot(`bool cannot have missingness` = !anyNA(bool))

    if (any(bool)) {
        df <- clean_observations(df, bool, "clean_invalid_ms_categories",
            "Invalid unnested values")
    }

    stopifnot(`Unnested questions not in qtable `=
        unique(df[["name"]]) %in% qtable[["name"]])

    return(df)
}

augment_df_with_expanded_ms <- function(df, ms_expanded, qtable) {

    info("Transform new question_ids for every new dichotomised variable")
    msRow <- nrow(ms_expanded)
    ms_expanded[["question_id"]] <- NULL
    ms_expanded <- merge(ms_expanded, qtable[, c("name", "question_id")],
        by = "name", all.x = TRUE)
    stopifnot(!anyNA(ms_expanded$question_id))
    stopifnot(msRow == nrow(ms_expanded))

    nms <- intersect(colnames(df), colnames(ms_expanded))
    out <- rbind.data.frame(
        df[0, nms], 
        ms_expanded[, nms])

    stopifnot(!anyNA(out$question_id))
    return(out)
}

main <- function(df, qtableTask, qtable) {

    nN <- unique(c(colnames(df), "name"))
    df <- merge_qtable(df, qtableTask)
    ms_expanded <- expand_ms(df, define_choices(df, qtableTask))
    out <- augment_df_with_expanded_ms(
        df = cleanup_df(df, ms_expanded, qtable),
        ms_expanded = ms_expanded,
        qtable = qtable)

    # Split out by unnested variable
    outList <- split(out, f = as.factor(out$name))
    stopifnot("list" %in% class(outList) & length(outList) == length(unique(out$name)))
    return(outList)
}

#
# Run functions
# --------------------------------------------------------------------------
if(no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    qtable <- load_qtable()
    qtableTask <- qtable[qtable$name == TASK_NAME, ]
    df <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]

    # Run
    collectedInputs <- named_list(df, qtableTask, qtable)
    do.call(main, collectedInputs) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done with unnesting multiple selection vars")
} else {
    # Tests
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_unnest_ms.R") %>%
		check_test_file_output()
}
