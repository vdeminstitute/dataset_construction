#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Clean vignettes
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
clean_by_vtable <- function(vdata, vtable) {

    bool <- !(vdata$question_id %in% vtable$vignette_id)
    vdata <- clean_observations(vdata, bool, "clean_by_vtable",
        "Removing vignettes not in vtable") %>% 
        dplyr::arrange(desc(id)) %>% 
        dplyr::distinct(question_id, coder_id, .keep_all = TRUE) %>%
        dplyr::ungroup()
    stopifnot(nrow(vdata) > 0)

    df <- merge(
        x = vtable[, c("vignette_id", "vignette_name", "parent_name",
            "threshold", "question_id", "hist_merged_id")],
        y = vdata,
        by.x = "vignette_id",
        by.y = "question_id",
        all.x = TRUE)

    # Transform select historical to contemporary vignettes
    df[["vignette_id"]] <- ifelse(
        !is.na(df[["hist_merged_id"]]),
        df[["hist_merged_id"]],
        df[["vignette_id"]])

    # Remove duplicates:
    df <- dplyr::arrange(df, desc(id)) %>%
        distinct(parent_name, vignette_id, coder_id, .keep_all = TRUE)
    stopifnot(nrow(df) > 0)

    return(df)
}

# Remove missing values from vignette data on key columns
clean_missing <- function(df) {

    bool <- is.na(df$code) | is.na(df$vignette_id) | is.na(df$coder_id)
    df <- clean_observations(df, bool, "clean_missing",
        "Removing missing values from vignette data")

    with(df, stopifnot(!anyNA(c(vignette_id, coder_id, code))))
    stopifnot(nrow(df) > 0)

    return(df)
}

# Remove vignette responses to match removed ratings
# -- vignette_id is an id per vignette, and there are almost always multiple vignettes per question_id
remove_responses <- function(df, vtable, remove_responses_list) {

    stopifnot("list" %in% class(remove_responses_list))

    # Remove vignette responses to match removed ratings
    for (i in names(remove_responses_list)) {
        bCode <- remove_responses_list[[i]]
        stopifnot(is.numeric(bCode))
        vid <- vtable$vignette_id[with(vtable, parent_name == i & !is.na(parent_name))]

        bool <- df$vignette_id %in% vid & df$code %in% bCode
        if (any(bool)) {
            df <- clean_observations(df, bool, "recode_responses",
                sprintf("Removing vignette responses for %s", i))
        }
        rm(bool)
    }

    return(df)
}

recode_responses <- function(df, recode_responses_list) {

    stopifnot("list" %in% class(recode_responses_list))

    # Recode observations in recode_responses_list
    for (i in names(recode_responses_list)) {
        info(sprintf("Recode vignette scores for %s", i))
        df[["code"]] <- ifelse(
            test = df[["parent_name"]] == i &
                df[["code"]] == recode_responses_list[[i]][["from"]],
            yes = recode_responses_list[[i]][["to"]],
            no = df[["code"]])
    }

    df <- df[, c("question_id", "parent_name", "vignette_id", "vignette_name",
        "coder_id", "code")]

    return(df)
}

clean_by_qtable <- function(df, qtable, vtable, remove_responses_list) {

    info("Vignettes outside coding values")
    inRow <- nrow(df)

    # Merge in cb_responses from qtable
    df <- merge(
        x = df,
        y = qtable[, c("question_id", "cb_responses", "k")],
        by = "question_id",
        all.x = TRUE)
    stopifnot(nrow(df) == inRow)
    df[["new_id"]] <- seq_along(df[["question_id"]])

    # Check for vignette responses outside coding values
    splits <- split(df, df$question_id, drop = TRUE)
    outside_id <- unlist(lapply(splits, function(x) {
        x <- x[!x$code %in% as.numeric(unlist(strsplit(x$cb_responses, " "))), ]
        x[["new_id"]]
    }))

    bool <- df$new_id %in% outside_id
    df <- clean_observations(df, bool, "clean_by_qtable",
        "Removing vignette responses outside coding values")
    df[["new_id"]] <- NULL

    # Shift by -1 to make 0 the lowest value
    # -- not for v2ellfelr as we are removing the top category
    remove_responses_list[["v2elffelr"]] <- NULL
    for (i in names(remove_responses_list)) {

        vid <- vtable[["vignette_id"]][vtable[["parent_name"]] == i]
        df[["code"]] <- ifelse(df[["vignette_id"]] %in% vid,
            df[["code"]] - 1, df[["code"]])
        }

    # Confirm that vignette responses respect k
    stopifnot(df[["code"]] <= (df[["k"]] - 1))

    # Keep only relevant columns and return distinct observations
    df <- df[, c("parent_name", "vignette_id", "vignette_name", "coder_id", "code")]
    res <- merge(
        x = df,
        y = unique(vtable[, c("id", "vignette_id")]),
        by = "vignette_id",
        all.x = TRUE)

    stopifnot(nrow(df)==nrow(res))
    return(res)
}

# Main function
main <- function(vtable, qtable, vdata) {
    info("Cleaning vignette data")

    # Lists that are used as input for the functions that remove or recode responses
    remove_responses_list <- list(v2mecenefi=0, v2lgdsadlo=0, v2elffelr=5)
    recode_responses_list <- list(v2jupack= list(from=4, to=3))

    info(sprintf("Removing vignette responses for %s", toString(names(remove_responses_list))))
    info(sprintf("Recoding vignette responses for %s", toString(names(recode_responses_list))))

    outdf <-
        clean_by_vtable(vdata, vtable) |> 
        clean_missing(df=_) |> 
        remove_responses(df=_, vtable, remove_responses_list) |> 
        recode_responses(df=_, recode_responses_list) |> 
        clean_by_qtable(df=_, qtable, vtable, remove_responses_list)
    
    return(outdf)
}


# Run script
if (no_test()) {

    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    vtable <- read_file(file.path(ROOT, "refs", "vignette_table.rds"))
    qtable <- load_qtable()
    objs <- find_dep_files(TASK_ID, db)
    vdata <- objs[[TASK_NAME]][[TASK_NAME]]

    # Run
    collectedInputs <- named_list(vtable, qtable, vdata)
    setNames(list(do.call(main, collectedInputs), DIRTYLIST), 
        c(TASK_NAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
    info("clean_vignetts is done.")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_clean_vignettes.R") %>%
		check_test_file_output()
}
