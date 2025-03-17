#!/usr/bin/env Rscript

# ==========================================================================
# Ordinalize indices.
# ==========================================================================
suppressMessages(library(vutils))
suppressMessages(library(dplyr))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
make_hli_df <- function(df, elmulpar, elfrfair, elecreg, utable) {
  hli_df <-
    Reduce(full_join_vdem, list(df, elmulpar, elfrfair, elecreg)) %>%
    add_date_cols %>%
    left_join(select(utable, country_text_id, year, gap_idx),
              by = c("country_text_id", "year")) %>% 
    group_by(country_text_id, gap_idx) %>%
    arrange(historical_date) %>%
    mutate(v2x_elecreg = locf(v2x_elecreg)) %>%
    ungroup %>% select(-gap_idx) %>%
  group_by(country_text_id) %>%
  arrange(year) %>%
  mutate(v2elmulpar_osp = ifelse(v2x_elecreg == 1, locf(v2elmulpar_osp), v2elmulpar_osp),
           v2elfrfair_osp = ifelse(v2x_elecreg == 1, locf(v2elfrfair_osp), v2elfrfair_osp)) %>%
    ungroup %>%
    arrange(country_text_id, year) %>% select(-historical_date)
}

ordinalise <- function(hli_df, vname) {
  hli_df[[paste0("e_", vname, "_3C")]] <-
    case_when(hli_df$v2x_elecreg == 0 ~ 0,
          hli_df[[vname]] <= 0.25 ~ 0,
          hli_df[[vname]] <= 0.5 & (hli_df$v2elmulpar_osp <= 2.5 | hli_df$v2elfrfair_osp <= 2) ~ 0,
          hli_df[[vname]] <= 0.5 & ((2.5 < hli_df$v2elmulpar_osp  & hli_df$v2elmulpar_osp <= 4) |
                        (2 < hli_df$v2elfrfair_osp & hli_df$v2elfrfair_osp <= 4)) ~ 0.5,
          hli_df[[vname]] > 0.5 & (2 < hli_df$v2elfrfair_osp & hli_df$v2elfrfair_osp < 3) ~ 0.5,
          hli_df[[vname]] > 0.5 & hli_df$v2elfrfair_osp >= 3 ~ 1)

  hli_df[[paste0("e_", vname, "_4C")]] <-
      case_when(hli_df$v2x_elecreg == 0 ~ 0,
              hli_df[[vname]] <= 0.25 ~ 0,
              hli_df[[vname]] <= 0.5 & (hli_df$v2elmulpar_osp <= 2 | hli_df$v2elfrfair_osp <= 2) ~ 0,
              hli_df[[vname]] <= 0.5 & ((2 < hli_df$v2elmulpar_osp & hli_df$v2elmulpar_osp <= 4) |
                          (2 < hli_df$v2elfrfair_osp & hli_df$v2elfrfair_osp <= 4)) ~ 1 / 3,
              hli_df[[vname]] > 0.5 & ((2 < hli_df$v2elmulpar_osp & hli_df$v2elmulpar_osp <= 3) |
                          (2 < hli_df$v2elfrfair_osp & hli_df$v2elfrfair_osp <= 3)) ~ 2 / 3,
              hli_df[[vname]] > 0.5 & (hli_df$v2elmulpar_osp > 3 & hli_df$v2elfrfair_osp > 3) ~ 1)

  hli_df[[paste0("e_", vname, "_5C")]] <- ord_5C(hli_df[[vname]])
  return(hli_df)
}

main <- function(df, elmulpar, elfrfair, elecreg, vname, utable) {
    hli_df <- make_hli_df(df, elmulpar, elfrfair, elecreg, utable) %>%
    ordinalise(., vname) %>%
    select(country_id, country_text_id, year,
        matches("^e_.*C$", ignore.case = FALSE))
  return(list(cy = hli_df))
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
  utable <- load_country_unit()
  objs <- find_dep_files(TASK_ID, db)
  vname <- gsub("^e_", "", TASK_NAME)
  df <- objs[[vname]][[vname]][["cy"]] %>% add_country_cols(country) %>%
      add_date_cols
  elmulpar <- objs[["v2elmulpar"]][["v2elmulpar"]][["cy"]] %>%
      add_country_cols(country) %>%
      add_date_cols
  elfrfair <- objs[["v2elfrfair"]][["v2elfrfair"]][["cy"]] %>%
      add_country_cols(country) %>%
      add_date_cols
  elecreg <- objs[["v2x_elecreg"]][["v2x_elecreg"]][["cy"]] %>%
      add_country_cols(country) %>%
      add_date_cols

    # Run
    collectedInputs <- named_list(df, elmulpar, elfrfair, elecreg, vname, utable)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)

    } else {
        testthat::test_file("~/proj/vdemds/module_unit_tests/index/test_ordinal_indices_els.R")
    }
