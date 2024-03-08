suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))
# --------------------------------------------------------------------------
# Functions
# --------------------------------------------------------------------------
compare_hosw <- function(df, country, utable) {

    stopifnot(!is.na(NEWEST_YEAR))

    df %<>%
        arrange(country_id, historical_date) %>%
        select(country_id, historical_date, year, v2ex_hosw, v2ex_hosw_calc,
            everything()) %>%
        left_join(select(country, country_id, country_name = name),
            by = "country_id") %>%
        select(country_name, country_id, year, historical_date, everything()) %>%
        mutate(
            diff =
            # no NA but calc is not equal to database version, or
                (!is.na(v2ex_hosw_calc) & !is.na(v2ex_hosw) &
                v2ex_hosw_calc != v2ex_hosw) |
            # database is missing but calculated is not
                (is.na(v2ex_hosw) & !is.na(v2ex_hosw_calc)),
            # Keep track of db missingness
            db_has_na = is.na(v2ex_hosw)
            )

    # LOCF within country-years
    df <- 
        add_gap_idx(df, utable) |>
        group_by(.data=_, country_id, gap_idx) |> 
        mutate(v2ex_hosw = locf(v2ex_hosw)) |> 
        ungroup()
    
    df %<>% select(country_name, country_id, year, historical_date, v2ex_hosw,
        v2ex_hosw_calc, diff, db_has_na, -country_text_id, everything())
    
    return(df)
}

main <- function(df, country, utable) {
    compare_hosw(df, country, utable)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()
    NEWEST_YEAR <- as.numeric(Sys.getenv("NEWEST_YEAR"))

    # Imports
    country <- load_country()
    utable <- load_country_unit()
    objs <- find_dep_files(TASK_ID, db)
    df <- lapply(names(objs), function(v) {
            if (v == "v2ex_hosw") {
                ddf <- objs[[v]][[v]]$cd %>%
                    add_country_cols(country) %>%
                    add_date_cols()
                new_scores_0101 <-
                    ddf %>% filter(historical_date == as.Date((NEWEST_YEAR - 1) %^% "-01-01")) %>%
                        mutate(
                            historical_date = as.Date(NEWEST_YEAR %^% "-01-01"),
                            year = NEWEST_YEAR)
                new_scores_1231 <-
                    ddf %>% filter(historical_date == as.Date((NEWEST_YEAR - 1) %^% "-12-31")) %>%
                        mutate(
                            historical_date = as.Date(NEWEST_YEAR %^% "-12-31"),
                            year = NEWEST_YEAR)
                out <-
                    bind_rows(ddf, new_scores_0101, new_scores_1231) %>%
                    arrange(country_id, historical_date)
                return(out)
            } else {
            objs[[v]][[v]]$cd %>%
                add_country_cols(country) %>%
                add_date_cols()
            }
        }) %>% Reduce(full_join_vdem, .)

    # Run
    collectedInputs <- named_list(df, country, utable)
    write_file(do.call(main, collectedInputs), gsub("[.]rds$", ".csv", OUTFILE),
        dir_create = TRUE)

    } else {
    # Call unit tests for main function and sub functions
    testthat::test_file("~/proj/vdemds/module_unit_tests/extra/test_compare_v2ex_hosw.R") 
    }
