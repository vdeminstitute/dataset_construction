#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))

set_env(MODULE_NAME = "divparctrl")

#
# Functions
# --------------------------------------------------------------------------
# Divided party control of legislature
divpartcl <- function(df) {
  df %<>%
      mutate(v2x_divparctrl =
          ifelse(!is.na(v2psnatpar_ord) & v2psnatpar_ord == 2,
                 v2psnatpar - 5, v2psnatpar) %>%
          scale %>% as.vector) %>%
      select(v2x_divparctrl, historical_date, country_text_id)
}

main <- function(df) {
  divpartcl(df)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
  # Global variables
  get_globals()

  # Imports
  objs <- find_dep_files(TASK_ID, DB)
  df <- lapply(names(objs), function(v) {
          objs[[v]][[v]]$cd
      }) %>%
      Reduce(partial(full_join,
                     by = c("country_id", "historical_date", "year")), .)

  # Run
  main(df) %>%
    list() %>%
    setNames(VARNAME) %>%
    write_file(., OUTFILE, dir_create = TRUE)

  } else {
    # Call unit tests for main function and sub functions
    testthat::test_file("~/proj/mm-prep/tests/extra/v2x_divparctrl.R")
  }
update_task_status(db = DB)
