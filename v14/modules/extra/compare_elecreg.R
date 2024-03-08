suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# ========================================================================== #
# In the script, we compare the database version of elecreg to what is calculated.
# The output of this is a product for A Data Team.
#
# This script relies on the environmental variable NEWEST_YEAR to be set. It does
# most comparisons between t and t-1, where t = NEWEST_YEAR. 
#
# Functions
# --------------------------------------------------------------------------
compare_elecreg <- function(df, country, NEWEST_YEAR) {

    stopifnot(!is.na(NEWEST_YEAR))

    df <-
        arrange(df, country_id, historical_date) |> 
        select(country_id, historical_date, year, v2x_elecreg, v2x_elecreg_calc,
            v2xex_elecreg, v2xex_elecreg_calc, v2xlg_elecreg, v2xlg_elecreg_calc) |> 
        left_join(select(country, country_id, country_name = name),
                by = "country_id") |>
        organiseRows(country_id, country_name, year, historical_date,
            foreground = TRUE)

    # Standard output    
    df_all_years <-
        mutate(df, diff =
            (v2x_elecreg & !is.na(v2x_elecreg) !=
                v2x_elecreg_calc & !is.na(v2x_elecreg_calc)) |
            (v2xlg_elecreg & !is.na(v2xlg_elecreg) !=
                v2xlg_elecreg_calc & !is.na(v2xlg_elecreg)) |
            (v2xex_elecreg & !is.na(v2xex_elecreg) !=
                v2xex_elecreg_calc & !is.na(v2xex_elecreg_calc)))

    # Calculate the difference in calculate valued between t-1 and t
    df_d_calc <-
        filter(df, year >= (NEWEST_YEAR - 1)) |> 
        group_by(country_id, year) |> 
        filter(year == NEWEST_YEAR | (year == (NEWEST_YEAR-1) & row_number() == n())) |> 
        ungroup() |> 
        group_by(country_id) |>  
        mutate(
            v2x_elecreg_calc_time_change =
                v2x_elecreg_calc != lag(v2x_elecreg_calc) &
                !is.na(lag(v2x_elecreg_calc)),
            v2xlg_elecreg_calc_time_change =
                v2xlg_elecreg_calc != lag(v2xlg_elecreg_calc) &
                !is.na(lag(v2xlg_elecreg_calc)),
            v2xex_elecreg_calc_time_change =
                v2xex_elecreg_calc != lag(v2xex_elecreg_calc) &
                !is.na(lag(v2xex_elecreg_calc))) |> 
        ungroup() |> 
        filter(year == NEWEST_YEAR) |> 
        select(country_id, historical_date, ends_with("time_change"))

    df_compare_last_obs <- 
        df |> 
        filter(year >= (NEWEST_YEAR - 1)) |> 
        group_by(country_id, year) |> 
        filter(row_number() == n()) |>  
        ungroup() |> 
        (\(dat) { 
            # Only t and t-1
            stopifnot(as.numeric(substr(dat$historical_date, 1, 4)) %in% c(
                NEWEST_YEAR, (NEWEST_YEAR-1)));
            # Last observation per year
            stopifnot(substr(dat$historical_date, 6, 10) == "12-31");
            dat
            })() |> 
        group_by(country_id) |> 
        mutate(v2x_elecreg = lag(v2x_elecreg)) |> 
        mutate(db_same_as_calc = v2x_elecreg == v2x_elecreg_calc) |> 
        ungroup() |> 
        filter(year == NEWEST_YEAR) |> 
        select(country_id, historical_date, db_same_as_calc)

    # Merge in the time_change columns
    df_out <-
        left_join(df_all_years, df_d_calc,
            by = c("country_id", "historical_date")) %>%
        left_join(x = ., y = df_compare_last_obs,
            by = c("country_id", "historical_date")) %>% 
    arrange(country_id, historical_date)

    return(df_out)
}

main <- function(df, country, NEWEST_YEAR) {
    compare_elecreg(df, country, NEWEST_YEAR)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    country <- load_country()
    objs <- find_dep_files(TASK_ID, db)
    df <- lapply(names(objs), function(v) objs[[v]][[v]]$cd) |>
        Reduce(full_join_vdem,  x =_) |> 
        organiseRows(country_id, historical_date)
    NEWEST_YEAR <- as.numeric(Sys.getenv("NEWEST_YEAR"))

    stopifnot(!is.na(NEWEST_YEAR))

    # Run
    collectedInputs <- named_list(df, country, NEWEST_YEAR)
    do.call(main, collectedInputs) %>%
        write_file(., gsub("[.]rds$", ".csv", OUTFILE), dir_create = TRUE)

} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/extra/tests_compare_elecreg.R")
}
