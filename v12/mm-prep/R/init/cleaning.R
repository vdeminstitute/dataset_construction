#!/usr/bin/env Rscript

# ==========================================================================
# Cleaning by variable, but also vignettes!
# ==========================================================================

suppressMessages(library(vutils))
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
set_env(MODULE_NAME = "cleaning")

#
# Functions
# --------------------------------------------------------------------------

remove_sequential <- function(df, VARNAME, qtable, coder) {
    stopifnot(VARNAME != "", !is.na(VARNAME))
	stopifnot(isTRUE(VARNAME %in% qtable$name) | VARNAME == "vignettes")

    info("Removing sequential codings...")
    if (qtable %>% filter(name == VARNAME) %>%
        pull(backfill_question) %T>%
        {stopifnot(!is.na(.))} %>%
        {!isTRUE(.)})
        return(df)

    df %<>% left_join(select(coder, coder_id, backfill_coder),
        by = "coder_id")
    bad_data <- df %>% filter(backfill_coder & year < 2005)
    info("Removing observations from sequential coding: " %^% nrow(bad_data))
    dirtylist[["sequentialCoding"]] <<- bad_data

    if (nrow(bad_data) > 0)
        info("Sequential coding removed observations: " %^% nrow(bad_data))

    df %>% 
        filter(is.na(backfill_coder) | !backfill_coder |
            (backfill_coder & year >= 2005)) %>%
        select(-backfill_coder) %>%
        return(.)
}

recoding_country_ids <- function(df) {
    stopifnot(nrow(df) > 0)
    info("Merge Chinas and Saudi Arabias")
    # Merge CHINA with China
    df$country_id[df$country_id == 213] <- 110
    # Merge Nejd with Saudi Arabia for historical merge
    df$country_id[df$country_id == 374] <- 197
    # Check Nejd in country_unit table!
    return(df)
}


merge_reference_tables <- function(df, qtable, country) {
    info("Merge reference tables...")
    df <- left_join_(df,
                 qtable[, c("question_id", "name", "class", "disaggregated",
                            "min", "code_col", "text_col", "question_type",
                            "cb_section")],
                 by = "question_id")

    ####
    info("Removing countries that we do not code:")
    ####
    # We must merge in the country table after we recoded the countries!
    df %<>% left_join_(country, by = "country_id")

    info("Is country_id anywhere NA except for vignettes and PSQ?")
    stopifnot(!(df$class != "V" &
              !grepl("^v\\dzz", df$name) &
              is.na(df$country_id)))

    return(df)
}

clean_codingstart_outside <- function(df, VARNAME) {
    stopifnot(nrow(df) > 0)
    info("Clean observations outside official coding periods...")
    bool <- 
        df$class != "V" &
        !grepl("^v\\dzz", df$name) &
        !is.na(df$country_id) &
        is.na(df$codingstart_contemp) &
        is.na(df$codingstart_hist)

    stopifnot(length(unique(df$id)) == nrow(df))
    ll <- remove_observations(df, bool)
    dirtylist[["outsideCodingStart"]] <<- ll$dirty
    info(VARNAME %^% " cleaning_id:001 remove countries by codingstart missing: " %^% 
        nrow(ll$dirty))
    return(ll$clean)
}

recode_and_remove_coders <- function(df, coders) {
    info("Merging historical coders who also coded contemporary data...")
    df %<>%
        mutate(coder_id = case_when(
            coder_id == 4248 ~ 636,
            coder_id == 4251 ~ 860,
            coder_id == 4252 ~ 1392,
            coder_id == 4387 ~ 419,
            coder_id == 4396 ~ 1674,
            coder_id == 4403 ~ 2914,
            coder_id == 4260 ~ 2979,
            coder_id == 4400 ~ 3074,
            coder_id == 5266 ~ 4080,
            coder_id == 5224 ~ 4527,
            coder_id == 5232 ~ 4541,
            coder_id == 5257 ~ 5525,
            TRUE ~ as.numeric(coder_id)))

    info("Removing test coders...")
    bad_coders <- coders %>% filter(test_coder) %$% coder_id
    # Some people are not actually bad coders but are flagged wrong in the interface
    bad_coders <- bad_coders[!bad_coders %in% c(1617, 5234, 4528,
        6106, 6105, 5598, 6104, 6107, 6103)]
    stopifnot(df %>% filter(class %in% c("A", "A*"), coder_id %in% bad_coders) %>% 
		nrow %>% {. == 0})

    df %<>% filter(is.na(coder_id) | !coder_id %in% bad_coders)

    return(df)
}

clean_hist_overlap <- function(df) {
    info("Dying historical countries overlap period.")
    info("Transform historical ovlerap-mm-data for expiring countries " %^%
     "to successsor country_ids...\n" %^%
     "Historical coders were asked to also code the years 1900-1920" %^%
     "for successor states in order to improve the mm-model.")

    bool <- df$class == "C" &
            !is.na(df$parent_country_id) &
            !is.na(df$end_date) &
            to_year(df$historical_date) > df$end_date
    df %<>% mutate(country_id = ifelse(bool, parent_country_id, country_id))

    info("There remains lots of non-C data for these countries from 1900-1920\n" %^%
     "This will be cleaned out with the utable cleaning...")

    return(df)
}

clean_text_answer <- function(df) {
    stopifnot(nrow(df) > 0)
    is.na(df$text_answer) <- df$text_answer == ""
    info("Removing new line characters in text_answer")
    df %>% mutate(text_answer = rm_newline(text_answer)) %>%
        return(.)
}

wrong_column <- function(df) {
    info("Is the data in the correct column?")
    info("These code questions have dirt in the text_answer column:")
    df %>% 
        filter(code_col) %>%
        filter(!is.na(text_answer)) %>%
        group_by(name, question_type) %>%
        summarize(n = n()) %>%
        arrange(desc(n)) %>%
        print

    info("These text_answer questions have dirt in the code column")
    df %>%
        filter(text_col) %>%
        filter(!is.na(code)) %>%
        group_by(name, question_type) %>%
        summarize(n = n()) %>%
        arrange(desc(n)) %>%
        print

    info("Let's clean those observations.")
    is.na(df$text_answer) <- df$code_col & !is.na(df$text_answer)
    is.na(df$code) <- df$text_col & !is.na(df$code)
    return(df)
}



juflow_recoding <- function(df) {
    stopifnot(nrow(df) > 0)
    info("v2juflow change input yes/no to 1/0...")
    # Note v2juflow has correct question_type and answer should be in code column
    df[df$question_id == 2798, "code"] <-
        as.numeric(df$text_answer[df$question_id == 2798] == "yes")
    is.na(df$text_answer) <- df$question_id == 2798
    return(df)
}

hist_elsnlsff_clrgunev_recoding <- function(df) {
    info("we're temporarily recoding v3clrgunev/v3elsnlsff since it was flipped.")
    if (!first(df$question_id) %in% c(2464, 2505))
        return(df)

    df %<>% mutate(code = case_when(
        (timestamp > "2017-06-15 14:53:31.308404") & (code == 0) ~ 2,
        (timestamp > "2017-06-15 14:53:31.308404") & (code == 2) ~ 0,
        TRUE ~ code))
    return(df)
}

B_recoding <- function(df) {
    stopifnot(nrow(df) > 0)
    info("Fixing B coding for China...")
    dates <- as.Date(c("1913-12-31", "1916-12-31", "1919-12-31", "1948-12-31"),
        "%Y-%m-%d")
    df$code[df$question_id == 560 & df$historical_date %in% dates] <- 0
    dates <- as.Date(c("1916-12-31", "1919-12-31", "1948-12-31"),
        "%Y-%m-%d")
    df$code[df$question_id == 564 & df$historical_date %in% dates] <- 1
    return(df)
}

jupack_recoding <- function(df) {
    stopifnot(nrow(df) > 0)
    info("v2jupack convert category 4 to 3")
    df$code[df$question_id == 580 & df$code == 4] <- 3
    return(df)
}

flag_historical_observations <- function (df) {
    stopifnot(!any(is.na(df[["name"]])))
    # Keep the data_type column for later when v2 and v3 are merged.
    df %>%
        mutate(data_type = ifelse(grepl("^v3", name), "historical", "normal")) %>%
        return(.)
}


main <- function(VARNAME, df, qtable, country, coder) {

    stopifnot(length(unique(df$id)) == nrow(df))
    country %<>% rename(country_name = name)
    
    df %>%
        remove_sequential(., VARNAME, qtable, coder) %>%
        recoding_country_ids(.) %>%
        merge_reference_tables(., qtable, country) %>%
        clean_codingstart_outside(., VARNAME) %>%
        recode_and_remove_coders(., coder) %>%
        clean_hist_overlap(.) %>% 
        juflow_recoding(.) %>%
        clean_text_answer(.) %>%
        wrong_column(.) %>% 
        hist_elsnlsff_clrgunev_recoding(.) %>%
        B_recoding(.) %>%
        jupack_recoding(.) %>%
        flag_historical_observations(.) %>%
        select(id, question_id, country_id, coder_id, historical_date, code, 
            text_answer, confidence, data_type, year) %>%
        untibble %>%
        return(.)
}


    

# Run script
if (no_test()) {
    # Global variables
    get_globals()
    dirtylist <- list()
   
    # Imports
    df <- find_dep_files(TASK_ID, DB)[[VARNAME]][[VARNAME]]
    qtable <- load_qtable()
    country <- load_country()
    coder <- read_file(file.path(ROOT, "download", "coder.rds"))

    # Run
    collectedInputs <- named_list(VARNAME, df, qtable, country, coder)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
    write_file(., OUTFILE, dir_create = T)
    info("Done first cleaning...")
    # stop()
} else {
    # Tests
    testthat::test_file("~/proj/mm-prep/tests/init/test_cleaning.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)

