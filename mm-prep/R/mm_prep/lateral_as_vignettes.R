#!/usr/bin/env Rscript

# ==========================================================================
# Recode lateral codes as vignettes, output the result as a separate file.
# ==========================================================================

options(warn = 2)

suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "lateral_as_vignettes")


#
# Define functions
# --------------------------------------------------------------------------
make_lateral_matrix <- function(matrix, coder_table) {
    stopifnot(is.matrix(matrix), is.data.frame(coder_table))
    var_coder_table <- coder_table[coder_table[["coder_id"]] %in% colnames(matrix),, drop = FALSE]

    ll <- split.data.frame(matrix,
                      factor(get_text_id(rownames(matrix)),
                        levels = unique(get_text_id(rownames(matrix)))))

    ml <- lapply(ll, function(m) {
        stopifnot(length(unique(get_text_id(rownames(m)))) == 1)
        country <- unique(get_text_id(rownames(m)))

        coders_df <- var_coder_table[var_coder_table[["country_text_id"]] == country,, drop = FALSE]

        lat_m <- imprint(m, colnames(m) %in% coders_df$coder_id[coders_df$lateral])

        return(lat_m)
        })

    lateral_matrix <- do.call(rbind, ml)
    stopifnot(nrow(matrix) == nrow(lateral_matrix),
        ncol(matrix) == ncol(lateral_matrix))
    return(lateral_matrix)
}

identify_lateral_coding_only_rows <- function(matrix, lateral_matrix) {
    nona <- !is.na(matrix)
    rows_to_delete <- which(rowSums(nona & lateral_matrix) == rowSums(nona)) %>% names

    if (length(rows_to_delete) > 0) {
    sprintf("We found %d lateral coder only rows to delete", length(rows_to_delete)) %>%
        warn

    print(rows_to_delete)
    }
    return(rows_to_delete)
}

remove_empty_cols <- function(input_data) {
    bool_mat <- !is.na(input_data$wdata)
    data_cols <- as.logical(colSums(bool_mat))
    input_data$wdata <- input_data$wdata[, data_cols, drop = FALSE]
    input_data$conf_mat <- input_data$conf_mat[, data_cols, drop = FALSE]
    return(input_data)
}


assert_input <- function(input_data) {
    stopifnot(is.list(input_data),
        length(input_data) == 3, 
        any(names(input_data) %in% c("wdata", "conf_mat", "country_dates")))
}

delete_lateral_coding_only_rows <- function(input_data, rows_to_delete) {
    assert_input(input_data)
    if (length(rows_to_delete) > 0) {
        input_data$wdata <- input_data$wdata[!rownames(input_data$wdata) %in% rows_to_delete, ]
        input_data$conf_mat <- input_data$conf_mat[rownames(input_data$wdata), ]
        input_data$country_dates <- input_data$country_dates[!input_data$country_dates %in% rows_to_delete]
    }
    return(input_data)
}

determine_lateral_rows <- function(matrix, lateral_m) {
    lateral_m[is.na(matrix)] <- FALSE
    is_lateral <- as.logical(rowSums(lateral_m))
    return(is_lateral)
}

extract_lateral_as_vignettes <- function(input_data, is_lateral_row) {
    if(!any(is_lateral_row))
        return(NULL)
    lateral_vignette <- input_data[["wdata"]][is_lateral_row,, drop = FALSE]
    rownames(lateral_vignette) <- paste0("A_", rownames(lateral_vignette))
    return(lateral_vignette)
}

set_lateral_to_na <- function(input_data, lateral_matrix) {
    is.na(input_data$wdata) <- input_data$wdata[lateral_matrix]
    is.na(input_data$conf_mat) <- input_data$wdata[lateral_matrix]
    return(input_data)
}

lateral_vignette <- function(input_data, is_lateral, lateral_matrix) {
    stopifnot(assert_input(input_data), 
        is.logical(is_lateral),     
        is.matrix(lateral_matrix))

    if (all(!is_lateral)) {
        input_data[["lateral_vignettes"]] <- 
            matrix(nrow = 0, ncol = ncol(input_data[["wdata"]]))
        colnames(input_data[["lateral_vignettes"]]) <- colnames(input_data[["wdata"]])
    } else {
        stopifnot(any(lateral_matrix))

        # Grab rows that include lateral coding
        lateral_vignette <- input_data[["wdata"]][is_lateral,, drop = FALSE]
        rownames(lateral_vignette) <- paste0("A_", rownames(lateral_vignette))
        # ???
        lateral_vignette[TRUE] <- FALSE

        # Remove lateral coding rows?
        input_data[["wdata"]] <- input_data[["wdata"]][!is_lateral,, drop = FALSE]
        input_data[["conf_mat"]] <- input_data[["conf_mat"]][!is_lateral,, drop = FALSE]
        input_data[["country_dates"]] <- input_data[["country_dates"]][!is_lateral,, drop = FALSE]

        input_data <- list(wdata = input_data[["wdata"]],
            conf_mat = input_data[["conf_mat"]],
            country_dates = input_data[["country_dates"]],
            lateral_as_vignettes = lateral_vignette)
    }

    return(input_data)
}

main <- function(input_data, coder_table, qtable, VARNAME) {
    # Remove lateral coding only rows
    lateral_matrix <- make_lateral_matrix(input_data$wdata, coder_table)
    rows_to_delete <- identify_lateral_coding_only_rows(input_data$wdata, lateral_matrix)
    input_data <- delete_lateral_coding_only_rows(input_data, rows_to_delete)
    input_data <- remove_empty_cols(input_data)

    # Transform lateral coding to vignettes
    lateral_matrix <- make_lateral_matrix(input_data$wdata, coder_table)
    is_lateral_row <- determine_lateral_rows(input_data$wdata, lateral_matrix)
    lat_vignettes <- extract_lateral_as_vignettes(input_data, is_lateral_row)
    input_data <- set_lateral_to_na(input_data, lateral_matrix)
    input_data[["lateral_vignettes"]] <- lat_vignettes
    return(input_data)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    objs <- find_dep_files(TASK_ID, DB)
    input_data <- objs[[VARNAME %^% "_interpolate_coders"]][[VARNAME]]
    coder_table <- objs[[VARNAME %^% "_coder_table"]][[VARNAME]]
    qtable <- load_qtable()

    # Run
    collectedInputs <- named_list(input_data, coder_table, qtable, VARNAME)
    do.call(main, collectedInputs) %>%
        list(.) %>%
        setNames(VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Writing input.data as lateral vignetter for " %^% VARNAME)

} else {
    # Call unit tests for main function and sub functions
    testthat::test_file("~/proj/mm-prep/tests/lateral_as_vignettes.R")
}
update_task_status(db = DB)