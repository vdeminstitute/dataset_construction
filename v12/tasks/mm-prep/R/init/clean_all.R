#!/usr/bin/env Rscript

# ==========================================================================
# Various cleaning procedures for all/most of the variables
# ==========================================================================

options(warn = 2)
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "clean_all")

#
# Functions
# --------------------------------------------------------------------------
merge_reference_tables <- function(df, qtable, country) {
    left_join_(df,
             qtable[, c("question_id", "name", "class",
                            "hist_outside_coding", "cont_outside_coding")],
                 by = "question_id") %>%
    left_join_(., select(country, country_id, country_name = name, codingstart_hist),
                 by = c("country_id"))
}

remove_historical_na <- function(df, VARNAME) {
    info("Remove observations where historical_date is NA. There should not be any.")
    res_ll <- remove_observations(df, is.na(df$historical_date))
    info(VARNAME %^% " cleaning_id:002 for historical_date is na: " %^% nrow(res_ll$dirty))
    stopifnot(!anyNA(res_ll[['clean']]$historical_date))
    dirtylist[["historicalNA"]] <<- res_ll$dirty
    return(res_ll$clean)
}

merge_country_unit <- function(df, country_unit) {
    # Clean Dates by utable (some exceptions may apply)
    # Note: Except left side for v3 A, A*, B variables.
    return(left_join_(df, select(country_unit, -historical_date), 
        by = c("country_id", "year")))
}

transform_historical <- function(df) {
    # Do all v3 variables have a codingstart_hist?
    stopifnot(all(!(grepl("^v3", df$name) & is.na(df$codingstart_hist))))

    # Lets identify a pre-historical time period because that is used for filling
    # for historical A A* B variables
    df$project[df$class != "C" &
           !is.na(df$codingstart_hist) &
           df$year < df$codingstart_hist] <- "pre-historical"


    info("Note: There are some observations for v3 variables prior to the " %^%
       "official coding period. These will be used for backfilling and are " %^%
       "then removed:")
    df %>%
        filter(project == "pre-historical") %>%
        print_by_country

    return(df)
}

clean_by_country_unit <- function(df, VARNAME) {
    stopifnot(!any(is.na(df$country_name)))

    info("Removing v2 observations outside our country-units.")
    bool <- grepl("^v2", df$name) &
          !df$cont_outside_coding &
          (is.na(df$project) |
              !df$project %in% c("overlap", "contemporary-only"))

    res_ll <- remove_observations(df, bool)
    df <- res_ll$clean
    info(VARNAME %^% " cleaning_id:003 for v2 observations outside utable: " %^%
      nrow(res_ll$dirty))
    dirtylist[["v2countryUnitCleaning"]] <<- res_ll$dirty
    rm(res_ll)

    info("Removing v3 observations outside our country-units (except pre-historical)")
    bool <- grepl("^v3", df$name) &
          !df$hist_outside_coding &
          (is.na(df$project) |
              !df$project %in% c("overlap", "historical-only", "pre-historical"))
    res_ll <- remove_observations(df, bool)
    df <- res_ll$clean
    info(VARNAME %^% " cleaning_id:004 v3 observations outside our country-units: " %^%
        nrow(res_ll$dirty))
    dirtylist[["v3countryUnitCleaning"]] <<- res_ll$dirty
    rm(res_ll)

    info("Removing v2/v3 observations outside our country-units for v2/3 extended!")
    bool <- grepl("^v\\d", df$name) &
          (df$hist_outside_coding | df$cont_outside_coding) &
          (is.na(df$project) |
              !df$project %in% c("overlap", "historical-only", "pre-historical", "contemporary-only"))
    res_ll <- remove_observations(df, bool)
    df <- res_ll$clean
    info(VARNAME %^% " cleaning_id:005 Removing v2/v3 observations outside our country-units for v3 extended: " %^%
        nrow(res_ll$dirty))
    dirtylist[["v23countryUnitCleaning"]] <<- res_ll$dirty
    # rm(res_ll)

    warn("Check which v2/v3 variables should have an exception, because they are coded longer.")

    return(df)
}

clean_duplicates <- function(df, VARNAME) {
    # There shouldn't be any cases left where project is NA
    stopifnot(!anyNA(df$project))

    # Clean duplicates
    # Did we grab any other class?
    stopifnot(df$class %in% c("A*", "A", "A,C", "B", "C", "D"))

    # Remove duplicates from A* A A,C B:
    info("Cleaning duplicates for A* A A,C B variables...")
    id_cols <- c("question_id", "country_id", "historical_date")
    res_ll <- remove_nonc_duplicates(df, id_cols)
    df <- res_ll$clean
    info(VARNAME %^% " cleaning_id:006 Cleaning duplicates for A* A A,C B variables: " %^%
         nrow(res_ll$dirty))
    dirtylist[["duplicatesA"]] <<- res_ll$dirty
    rm(res_ll)

    stopifnot(length(unique(df$id)) == nrow(df))

    # Remove duplicates from C data
    info("Cleaning duplicates for C variables...")
    id_cols <- c("question_id", "country_id", "historical_date", "coder_id")
    res_ll <- remove_c_duplicates(df, id_cols)
    df <- res_ll$clean
    info(VARNAME %^% " cleaning_id:007 Cleaning duplicates for C variables: " %^%
        nrow(res_ll$dirty))
    dirtylist[["duplicatesC"]] <<- res_ll$dirty
    stopifnot(no_duplicates(df, c("country_id", "historical_date", "coder_id")))
    return(df)
}

percent_cleaning <- function(df, qtable, VARNAME) {
    df %<>%
          left_join_(qtable[, c("question_id", "question_type", "choice_values")],
               by = "question_id")

    info("Clean out of bounds percentage values")
    bool <- df$question_type == "R" & (df$code > 100 | df$code < 0)

    res_ll <- remove_observations(df, bool)
    df <- res_ll$clean
    info(VARNAME %^% " cleaning_id:008 Clean percentag variables: " %^%
        nrow(res_ll$dirty))
    dirtylist[["percentCleaning"]] <<- res_ll$dirty
    return(df)
}

mecenefi_cleaning <- function(df, VARNAME) {
    stopifnot(nrow(df) > 0)
    if (VARNAME != "v2mecenefi")
        return(df)

    info("Cleaning v2mecenefi...")

    # Special cleaning for v2mecenefi. No internet < 1993 so drop all
    # rows. For each year after check if >=50% of coders claim internet
    # existed, otherwise drop year
    mecenefi.df <-
        df %>%
        filter(name == "v2mecenefi") %>%
        filter(historical_date >= '1993-01-01')

    years <-
        mecenefi.df %>%
        group_by(coder_id, country_id, year) %>%
        summarise(code = max(code)) %>%
        group_by(country_id, year) %>%
        filter(sum(code == 0) / length(code) >= .5) %>%
        select(country_id, year)

    mecenefi.df %<>%
        anti_join(years, by = c("country_id", "year")) %>%
        select(-year)

    df %<>% filter(name != "v2mecenefi") %>% bind_rows(mecenefi.df)
    return(df)
}


percent_multiply <- function(df) {

    df %>% 
        mutate(code = ifelse(
            name %in% c("v2msuffrage", "v2fsuffrage", "v2asuffrage"),
            code * 100,
            code)) %>%
        return(.)
}

clean_confidence <- function(df, VARNAME) {
    # What about questions that don't have any confidence?
    badConf <- df %>% filter(confidence > 100)
    stopifnot(nrow(badConf) == 0)

    info("Clean where class is C and confidence is zero...")
    bool <- df$class == "C" & df$confidence == 0

    res_ll <- remove_observations(df, bool)
    df <- res_ll$clean
    info(VARNAME %^% " cleaning_id:009 class C confidence 0: " %^%
        nrow(res_ll$dirty))
    dirtylist[["zeroConfC"]] <<- res_ll$dirty
    rm(res_ll)

    stopifnot(!anyNA(df$project))
    stopifnot(!any(!is.na(df$code) & df$code < 0))
    return(df)
}

main <- function(VARNAME, qtable, country, country_unit, df) {
    stopifnot(length(unique(df$id)) == nrow(df))
    df %>%
        merge_reference_tables(., qtable, country) %>%
        remove_historical_na(., VARNAME) %>%
        merge_country_unit(., country_unit) %>%
        transform_historical(.) %>%
        clean_by_country_unit(., VARNAME) %>%
        clean_duplicates(., VARNAME) %>%
        percent_cleaning(., qtable, VARNAME) %>%
        mecenefi_cleaning(., VARNAME) %>%
        percent_multiply(.) %>%
        clean_confidence(., VARNAME) %>%
        select(id, question_id, country_id, coder_id, historical_date, code, 
            text_answer, confidence, year, data_type) %>%
        return(.)
}

# Run script
if (no_test()) {
    # Global variables
    get_globals()
    dirtylist <- list()

    # Imports
    qtable <- load_qtable()
    country <- load_country()
    country_unit <- load_country_unit()
    df <- find_dep_files(TASK_ID, DB)[[VARNAME]][[VARNAME]]

    # Run
    collectedInputs <- named_list(VARNAME, qtable, country, country_unit, df)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with clean_all")
} else {
    testthat::test_file("~/proj/mm-prep/tests/init/test_clean_all.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)
