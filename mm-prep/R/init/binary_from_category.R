#!/usr/bin/env Rscript

# ==========================================================================
# Create binary variables from baseline categories
# [Note: we must remove those categories from the original variables!]
# Check coding interface as well as codebook!
# ==========================================================================

options(warn = 2)
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "binary_from_category")

#
# Functions
# --------------------------------------------------------------------------
append_df <- function(df, qtable) {
  df %<>% left_join(select(qtable, question_id, name, recode_category_to_binary),
                  by = "question_id")
  stopifnot(all(!is.na(df$recode_category_to_binary)))
  return(df)
}

binary_version <- function(df, qtable) {
    df %>%
      mutate(code = ifelse(code == recode_category_to_binary, 0, 1),
           name = name %^% "bin") %>%
      mutate(question_id =
         trans(name, to = "question_id", ttable = qtable, by = "name"))
}

binary_transform <- function(df) {
    # Remove answer category
    df %>% filter(code != df$recode_category_to_binary) %>%
    # Shift baseline down to zero! (necessary for the measurement model :)
    # mecenefi and lgdsadlo lost their baseline 0 category.
    mutate(code = ifelse(name %in% c("v2mecenefi", "v2lgdsadlo"),
                             code - 1, code)) %>%
    select(-recode_category_to_binary) %>%
    return(.)
}

main <- function(qtable, df) {
    df <- append_df(df, qtable)
    return(list(binary_transform(df), binary_version(df, qtable)))
}

# Run script
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    qtable <-  load_qtable()
    objs <- find_dep_files(TASK_ID, DB)
    df <- objs[[VARNAME]][[VARNAME]]

    # Run
    collectedInputs <- named_list(qtable, df)
    setNames(do.call(main, collectedInputs), 
        c(VARNAME, VARNAME %^% "bin")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with binary_from_category...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/init/test_binary_from_category.R")
}
update_task_status(db = DB)
