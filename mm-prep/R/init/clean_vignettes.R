#!/usr/bin/env Rscript

# ==========================================================================
# Let's clean some vignettes!
# ==========================================================================

options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "clean_vignettes")

#
# Functions
# --------------------------------------------------------------------------
clean_by_vtable <- function(vdata_df, vtable) {
    info("Filter out vignettes outside vtable")
    df <- filter(vdata_df, question_id %in% vtable$vignette_id)
    stopifnot(nrow(df) > 0)

    info("Clean dups dings. N rows: " %^% nrow(df))
    df %<>%
        arrange(desc(id)) %>%
        distinct(question_id, coder_id, .keep_all = TRUE) %>%
        ungroup()
    info("Now N rows:" %^% nrow(df))
    info("Join vtable. N cols: " %^% ncol(df) %^% "; N rows: " %^% nrow(df))
    df <-
    left_join_(vtable[, c("vignette_id", "vignette_name", "parent_name",
                          "threshold", "question_id", "hist_merged_id")],
               df, by = c("vignette_id" = "question_id"))
    info("Now N rows: " %^% nrow(df) %^% "; N cols: " %^% ncol(df))
    # Transform select historical to contemporary vignettes
    df %<>% mutate(vignette_id = ifelse(!is.na(hist_merged_id),
                                    hist_merged_id,
                                    vignette_id))

    df %>%
        arrange(desc(id)) %>%
        distinct(parent_name, vignette_id, coder_id, .keep_all = T) %>%
        return(.)
}

clean_missing <- function(df) {
    df %<>% filter(!is.na(coder_id), !is.na(code), !is.na(vignette_id))

    with(df, stopifnot(!anyNA(c(vignette_id, coder_id, code))))
    stopifnot(nrow(df) > 0)
    return(df)
}

change_codes <- function(df, vtable, baseline_vars) {
    # We remove baseline category from these C vars; do the
    # same with vignettes
    for (b in names(baseline_vars)) {
        info("Cleaning baseline var: " %^% b)
        vignette_ids <- filter(vtable, parent_name == b) %$% vignette_id
        df %<>% filter(!(vignette_id %in% vignette_ids & code == baseline_vars[b]))
        # v2elffelr vignette does not include category 5
    }

    # v2jupack 4 -> 3
    df %<>% mutate(code = ifelse(parent_name == "v2jupack" & code == 4, 3, code))
    df %>% select(question_id, parent_name, vignette_id, vignette_name, 
        coder_id, code) %>%
        return(.)
}

clean_by_qtable <- function(df, qtable, vtable, baseline_vars) {
    info("Vignettes outside coding values")
    df %<>% left_join_(qtable[, c("question_id", "cb_responses")]) %>%
        mutate(new_id = seq_along(question_id))

    dirty_id <- df %>%
    group_by(question_id) %>%
    filter(!code %in%
             as.numeric(unlist(strsplit(first(cb_responses), " ")))) %>%
    pull(new_id)

    df %>% filter(new_id %in% dirty_id) %>%
        group_by(parent_name, code) %>%
        summarize(n = n()) %>%
        as.data.frame(stringsAsFactors = F) %>%
        print()

    df %<>% filter(!new_id %in% dirty_id) %>%
        select(-new_id)

    # Base categories back to 0
    for (b in names(baseline_vars)) {
        if (b != "v2elffelr") {
            vignette_ids <- filter(vtable, parent_name == b) %>%
                pull(vignette_id)
           df %<>% mutate(code = ifelse(vignette_id %in% vignette_ids,
                                         code - 1, code))
        }
    }
    return(df)
}

final_prep <- function(df, qtable, vtable) {
    df %<>% left_join_(qtable[, c("question_id", "k")], by = "question_id")
    stopifnot(df$code <= (df$k - 1))
    # Check if any are less than qtable$k per parent_name
    df %<>% select(parent_name, vignette_id, vignette_name, coder_id, code)
    df %>% left_join_(distinct(vtable, id, vignette_id), by = "vignette_id") %>%
        return(.)
}

main <- function(vtable, qtable, vdata_df, VARNAME) {
    info("Cleaning vignette data")
    # Adjust for change in codes!
    baseline_vars <- list("v2mecenefi" = 0, "v2lgdsadlo" = 0, "v2elffelr" = 5)

    clean_by_vtable(vdata_df, vtable) %>%
        clean_missing(.) %>%
        change_codes(., vtable, baseline_vars) %>%
        clean_by_qtable(., qtable, vtable, baseline_vars) %>%
        final_prep(., qtable, vtable) %>%
        return(.)
}




# Run script
if (no_test()) {

    # Global variables
    get_globals()
    dirtylist <- list()

    # Imports
    vtable <- read_file(file.path(ROOT, "refs", "vignette_table.rds"))
    qtable <- load_qtable()
    objs <- find_dep_files(TASK_ID, db)
    vdata_df <- objs[[VARNAME]][[VARNAME]]

    # Run
    collectedInputs <- named_list(vtable, qtable, vdata_df, VARNAME)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with clean_vignettes...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/init/test_clean_vignettes.R")
}
update_task_status(db = DB)