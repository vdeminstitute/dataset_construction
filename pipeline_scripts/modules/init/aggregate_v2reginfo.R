#!/usr/bin/env Rscript

# ==========================================================================
# Extract information from v2reginfo and split it into 2 variables:
# v2regdur (regime duration) and v2regidnr (regime ID)
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

check_name <- function(TASK_NAME) {

    stopifnot(`TASK_NAME needs to be either v2regdur or v2regidnr` =
        TASK_NAME %in% c("v2regdur", "v2regidnr"))

}

# Function that creates an new variable called v2regdur
create_duration_var <- function(df) {

    stopifnot(`df needs to a data.frame` = is.data.frame(df))
    info("Creating v2regdur from v2reginfo")

    bool <- is.na(df[["text_answer"]])
    stopifnot(`bool cannot have missingness` = !anyNA(bool))

    if (any(bool)) {
        df <- clean_observations(
            df = df,
            bool = bool,
            function_name = "create_duration_var",
            description = "missing value on text_answer")
    }

    stopifnot(`text_col lacks a date-like structure` = grepl(
        pattern = "\\d{2}/\\d{2}/\\d{4}\\s[-–—−―]\\s\\d{2}/\\d{2}/\\d{4}|E",
        x = df[["text_answer"]]))
    
    df_out <- df |> 
        arrange(country_id, historical_date) |>  
        select(country_id, historical_date, text_answer, year) |> 
        mutate(
            reg_start = gsub(
                pattern = "(.*\\()(\\d{2}/\\d{2}/\\d{4}\\s?)(.*)",
                replacement = "\\2",
                x = text_answer),
            reg_end = gsub(
                pattern = "(.*[-–—−―] )(\\d{2}/\\d{2}/\\d{4})(\\))",
                replacement = "\\2",
                x = text_answer)) |> 
        mutate(
            across(starts_with("reg_"), ~as.Date(.x, format = "%d/%m/%Y"))) |> 
        mutate(
            historical_date = case_when(
                country_id %in% c(28, 60, 71, 116) & historical_date == as.Date("2023-01-01") ~ reg_start,
                TRUE ~ historical_date
            )) |> 
        mutate(
            code = ifelse(
                test = historical_date <= reg_end | is.na(reg_end),
                yes = historical_date - reg_start,
                no = 0))

    # code is number of days, so it should not be less than 0 or missing.
    stopifnot(!is.na(df_out[["code"]]) & df_out[["code"]] >= 0)
    # for all code == 0, historical_date >= reg_start must hold
    stopifnot(
        df_out[df_out[["code"]]==0, "historical_date"] >=
        df_out[df_out[["code"]]==0, "reg_start"])
    # Within this function, we want rows of df to equal rows of out_df
    stopifnot(nrow(df) == nrow(df_out))

    df_out <-
		select(df_out, country_id, historical_date, code, year) |> 
		arrange(country_id, historical_date)

    return(df_out)
}

# Function to extract v2regidnr from v2reginfo
create_regid_var <- function(df) {

    stopifnot(`df needs to be a data.frame` = is.data.frame(df))
    info("Creating v2regidnr from v2reginfo")

    bool <- is.na(df[["text_answer"]])
    stopifnot(`bool cannot have missingness` = !anyNA(bool))

    if (any(bool)) {
        df <- clean_observations(
            df = df,
            bool = bool,
            function_name = "create_regid_var",
            description = "missing value on text_answer")
    }

    df_out <-
        arrange(df, country_id, historical_date) |> 
        distinct(country_id, text_answer) |> 
        group_by(country_id) |>
        mutate(idcode = as.integer(sprintf("%d%02d", country_id, 1:n()))) |>
        ungroup() |>
        inner_join(x=_, y=df, by = c("country_id", "text_answer")) |>
        select(country_id, historical_date, code = idcode, year) |>
		arrange(country_id, historical_date)

    # There should be no missingness on code
    stopifnot(!is.na(df_out[["code"]]))
    # No rows should be added within this function
    stopifnot(nrow(df) == nrow(df_out))

    return(df_out)
}

# Create the output that we want
create_regime_variables <- function(df, TASK_NAME) {

    stopifnot(is.data.frame(df))
    stopifnot(is.character(TASK_NAME) & length(TASK_NAME)==1)

    if (TASK_NAME == "v2regdur") {
        df <- create_duration_var(df)
    }

    if (TASK_NAME == "v2regidnr") {
        df <- create_regid_var(df)
    }

    return(df)
}

# Function to merge question_id information to the final data.frame
merge_qtable <- function(df, qtable, TASK_NAME) {

    out_df <-
        filter(qtable, name == TASK_NAME) |> 
        select(question_id) |> 
        bind_cols(df)

    stopifnot(nrow(df) == nrow(out_df))
    stopifnot(ncol(df) + 1 == ncol(out_df))

    return(out_df)
}


# Main
main <- function(df, qtable, TASK_NAME) {

    check_name(TASK_NAME)

    df_out <- 
        create_regime_variables(df, TASK_NAME) |> 
        merge_qtable(df=_, qtable, TASK_NAME)

    return(df_out)
}


# Run script
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()
    
    # Imports
    qtable <- load_qtable()
    df <- find_dep_files(TASK_ID, db)[["v2reginfo"]][["v2reginfo"]]

    # Run
    collectedInputs <- named_list(df, qtable, TASK_NAME)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Created " %^% TASK_NAME)

} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_aggregate_v2reginfo.R") %>%
		check_test_file_output()
}
