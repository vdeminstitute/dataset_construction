#!/usr/bin/env Rscript

# ==========================================================================
# Merge contemporary and historical versions of variables
# ==========================================================================

options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "cont_hist_merge")

#
# Functions
# --------------------------------------------------------------------------
merge_ref_tables <- function(df, qtable, country_unit, country) {
    stopifnot(!with(df, anyNA(question_id, country_id)))
    df %<>% left_join_(qtable[, c("question_id",
                            "hist_merge_conflict", "hist_merge_no_conflict",
                            "class", "name")],
                 by = "question_id")

    df %<>% left_join_(country_unit[, c("country_id", "year", "project")],
                   by = c("country_id", "year"))

    df %<>% left_join_(country[, c("country_id", "codingstart_hist")],
                   by = "country_id")
    return(df)
}

data_checks <- function(df) {
    # Adjust project for pre-historical data
    df$project[df$class != "C" &
          !is.na(df$codingstart_hist) &
          df$year < df$codingstart_hist] <- "pre-historical"
    stopifnot(!anyNA(df$project))

    # Are the variables normal v2 or v3 variables?
    stopifnot(grepl("^v\\d", df$name))

    n_overlap_hist <- df %>%
        filter(class != "C") %>%
        filter(hist_merge_conflict | hist_merge_no_conflict) %>%
        filter(project %in% c("overlap")) %>%
        nrow

    n_overlap_cont <- df %>%
        filter(class != "C") %>%
        filter(grepl("^v2", name)) %>%
        filter(project %in% c("overlap")) %>%
        nrow

    if (length(n_overlap_hist) > 0 &&
        length(n_overlap_cont) > 0 &&
        (n_overlap_hist > n_overlap_cont)) {
            msg <- "historical overlap has more observations than contemporary! " %^%
            "hist: " %^% n_overlap_hist %^% "; " %^% "cont: " %^% n_overlap_cont
            warn(msg)
    }

    return(df)
}

extractv3 <- function(df, VARNAME, qtable) {
    df %<>% filter(grepl("^v3", name))
    stopifnot(df$hist_merge_conflict | df$hist_merge_no_conflict)

    if (first(df$class) != "C") {
        stopifnot(grepl("^v2", VARNAME))
        if (qtable %>% filter(name == VARNAME) %$% overlap_use_hist == TRUE) {
            df %<>%
                filter(project %in% c("pre-historical", "historical-only", 
                    "overlap"))
        } else {
            df %<>%
                filter(project %in% c("pre-historical", "historical-only"))
        }
    }

    if (first(df$class) == "C") {
        df %<>%
            filter(project %in% c("historical-only", "overlap"))
    }

    stopifnot(grepl("^v3", df$name))
    return(df)
}

transform_qids <- function(df, qtable) {
    df %<>%
        mutate(question_id = normalize_qids(question_id, qtable))
    stopifnot(!2446 %in% df$question_id)
    stopifnot(!any(is.na(df$question_id)))
    return(df)
}

extractv2 <- function(df, VARNAME, qtable) {
    v2merge <- df %>% filter(grepl("^v2", name))
    if (qtable %>% filter(name == VARNAME) %$% overlap_use_hist == TRUE) {
        v2merge %<>% filter(!project %in% c("overlap"))
    }
    return(v2merge)
}

remove_duplicates <- function(df) {
    # Check for duplicates after merging!
    # There should not be any!
    ####
    df$id <- seq_along(df$id)

    # Remove duplicates from A* A A,C B:
    info("Cleaning duplicates for A* A A,C B variables...")
    id_cols <- c("question_id", "country_id", "historical_date")
    res_ll <- remove_nonc_duplicates(df, id_cols)
    df <- res_ll$clean
    dirt <- res_ll$dirty
    rm(res_ll)

    # Remove duplicates from C data
    info("Cleaning duplicates for C variables...")
    id_cols <- c("question_id", "country_id", "historical_date", "coder_id")
    res_ll <- remove_c_duplicates(df, id_cols)
    df <- res_ll$clean
    dirt <- bind_rows(dirt, res_ll$dirty)
    rm(res_ll)

    stopifnot(no_duplicates(df, c("country_id", "historical_date", "coder_id")))
    return(df)
}

eltype_recoding <- function(df, VARNAME) {
    stopifnot(nrow(df) > 0)
    if (VARNAME != "v2eltype")
        return(df)
    # Remove rows of v2eltype where the merged in v3eltype brought 2,3,8,9
    # as elections. We can then use the new v2eltype to clean the other v2el
    # variables (before unnesting v2eltype)
    
    # extract text_answer elements
    df %<>% mutate(text_answer = gsub("2|3|8|9", "", text_answer))

    # Remove double commas
    while (any(grepl(",,", df$text_answer))) {
        df %<>% mutate(text_answer = gsub(",,", ",", text_answer))
    }

    # remove leading or trailing commas
    df %<>%
        mutate(text_answer = trimws(df$text_answer, "both", ","))

    # There should not by an NAs
    stopifnot(!any(is.na(df$text_answer)))
    # Remove rows that have only empty string
    df %>% filter(text_answer != "") %>%
        return(.)
}

lgbicam_recoding <- function(df, VARNAME) {
    stopifnot(nrow(df) > 0)
    if (VARNAME != "v2lgbicam")
        return(df)
    # Recode lgbicam to fit contemporary
    # Ensure merged lgbicam conforms to contemporary coding rules.
    #  9 -> "Other legislatures" (ex: Monarch's advisory council), set to 0
    #  >2 -> Multichamber legislatures
    df[df$question_id == 595 & df$code == 9, "code"] <- 0
    df[df$question_id == 595 & df$code > 2, "code"] <- 2
    return(df)
}

main <- function(VARNAME, df, qtable, country_unit, country) {
    qtable %<>% filter(name %in% c(VARNAME, gsub("^v2", "v3", VARNAME)))
    if (all(qtable$class == "C") & length(unique(qtable$k)) != 1) {
        stop("Contemporary and historical have different k!")
    }
    country %<>% rename(country_name = name)
    country_unit %<>% select(-historical_date)
    n <- colnames(df)
    warn("v3elage")
    df <- 
        merge_ref_tables(df, qtable, country_unit, country) %>%
        data_checks(.)
    v3 <- extractv3(df, VARNAME, qtable) %>% transform_qids(., qtable)
    v2 <- extractv2(df, VARNAME, qtable)
    bind_rows(v2, v3) %>%
        remove_duplicates(.) %>%
        eltype_recoding(., VARNAME) %>%
        lgbicam_recoding(., VARNAME) %>%
        select(., one_of(n), -project) %>%
        return(.)
}

# Run script
if (no_test()) {
    # Global variables
    get_globals()
    dirtylist <- list()

    # Imports
    country_unit <- load_country_unit()
    country <- load_country()
    qtable <- load_qtable()
    objs <- find_dep_files(TASK_ID, DB)
    df <- bind_rows(objs[[VARNAME]][[VARNAME]],
        objs[[gsub("v2", "v3", VARNAME)]][[gsub("v2", "v3", VARNAME)]])

    # Run
    collectedInputs <- named_list(country_unit, country, qtable, df, VARNAME)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with cont_hist_merge...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/downstream_cleaning/test_cont_hist_merge.R")
}
update_task_status(db = DB)