#!/usr/bin/env Rscript

# ==========================================================================
# elecoff.R: creates v2x_elecoff + related indices
#
# Requires CY versions:
#     - elecreg
#     - Cleaned A-vars
#     - MM output
#
# This is a direct translation of a stata script hence the
# comments...
# ==========================================================================

options(warn = 2)

suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "elecoff")

#
# Functions
# --------------------------------------------------------------------------
cd_df <- function(input.df) {
  # Please forgive me.
  attach(input.df)

  # Construct hosw_rec and hogw_rec (relative power of hos/hog)
  #
  # Compare HOS vs HOG power appoint cabinet and dismiss ministers.
  #
  # A couple issues:
  #     1. The 0.5 category will practically never be evaluated as TRUE;
  #        the probability of the MM output for, as an example,
  #        v2exdfdbhs_rec and v2exdjcbhg being exactly the same is so
  #        infinitesimally small that this condition is redundant.
  #     2. Shouldn't we be using the OSP version of these
  #        variables?
  #     3. There's a logical inconsistency with how we're handling NA
  #        here and what gets set later as hogw_rec. Essentially,
  #        hosw_rec gets set when the HOS variable is NOT missing and
  #        the HOG variable is; however, the opposite is not true for
  #        hogw_rec.
  #     4. Lastly, these NA checks are a holdover from the Stata script;
  #        they should be mostly fixed already by downstream cleaning,
  #        but we'll keep them for now.
  v2ex_hogw <- 1 - v2ex_hosw

  ###
  # HOS/HOG appointed by legislative & HOG appointed by HOS
  #
  # In Stata, NA | F = F, but in R, NA | F = NA. Since this is a
  # translation of a Stata script, keep the old functionality :(.
  v2ex_legconhos <- ((!is.na(v2expathhs) & v2expathhs == 6) | (!is.na(v2exaphos) & v2exaphos == 1))
  v2ex_legconhog <- ((!is.na(v2expathhg) & v2expathhg == 7) |
                    (!is.na(v2exaphogp) & v2exaphogp == 1)) &
                    v2exhoshog == 0

  v2ex_hosconhog <- v2expathhg == 6

  ###
  # Legislature directly elected
  v2xex_elecleg <- ifelse((v2exapup == 1 | v2exapupap == 1) & !is.na(v2lgelecup) & !is.na(v2lginelup),
                        ((v2lgello + v2lginello / 2) + (v2lgelecup + v2lginelup / 2)) / 2,
                        v2lgello + v2lginello / 2) / 100

  dual <- v2exhoshog == 0

  ###
  # Finally, elected officials index
  accleghos <- v2xex_elecleg * v2ex_legconhos
  accleghog <- ifelse(dual, v2xex_elecleg * v2ex_legconhog, NA)
  acchoshog <- ifelse(dual, v2ex_elechos * v2ex_hosconhog, NA)
  accleghoshog <- ifelse(dual, v2xex_elecleg * v2ex_legconhos * v2ex_hosconhog, NA)

  acchog_dual <- ifelse(dual, pmax(acchoshog, accleghoshog, v2ex_elechog, accleghog), NA)
  accex <- ifelse(dual,
                 v2ex_hosw * pmax(v2ex_elechos, accleghos) + v2ex_hogw * acchog_dual,
                 pmax(v2ex_elechos, accleghos))


  # Penalize systems with directly elected presidents
  v2x_elecoff <- ifelse(v2ex_elechos == 1 & v2ex_hosw == 1, (accex + v2xex_elecleg) / 2, accex)


  elecoff_cd.df <- data.frame(country_text_id, historical_date = historical_date,
                             v2x_elecoff, v2ex_hosw, v2ex_hogw, v2ex_legconhos,
                             v2ex_legconhog, v2ex_hosconhog, v2xex_elecleg,
                             stringsAsFactors = F) %>%
      mutate_if(is.logical, as.numeric)
  detach(input.df)
  return(elecoff_cd.df)
}

cy_df <- function(elecoff_cd.df) {
  ratio.df <- select(elecoff_cd.df, country_text_id, historical_date, v2xex_elecleg) %>%
    cy.day_mean(dates = historical_date, by = country_text_id)

  rest.df <- select(elecoff_cd.df, -v2xex_elecleg) %>%
      mutate(year = to_year(historical_date), historical_date = NULL) %>%
      group_by(country_text_id, year) %>%
      summarise_all(collect_last) %>%
      ungroup

  elecoff_cy.df <- full_join(rest.df, ratio.df, by = c("country_text_id", "year"))
}

make_df <- function(elecoff_cd.df, elecoff_cy.df) {
  out <- list()
  vars <- c("v2x_elecoff", "v2ex_hogw", "v2ex_legconhos",
          "v2ex_legconhog", "v2ex_hosconhog", "v2xex_elecleg")
  lapply(vars, function(v) {
    out[[v]]$cd <<- elecoff_cd.df[, c("country_text_id", "historical_date", v)]

    out[[v]]$cy <<- elecoff_cy.df[, c("country_text_id", "year", v)]

    }) %>% invisible
  return(out)
}

main <- function(input.df) {
    elecoff_cd.df <- cd_df(input.df)
    elecoff_cy.df <- cy_df(elecoff_cd.df)
    return(make_df(elecoff_cd.df, elecoff_cy.df))
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
  # Global variables
  get_globals()

  # Imports
  objs <- find_dep_files(TASK_ID, DB)
  utable <- load_country_unit()
  country <- load_country()

  df <- lapply(names(objs), function(v) {
            objs[[v]][[v]]$cd
        }) %>%
        Reduce(partial(full_join,
                       by = c("country_id", "historical_date", "year")), .)
  df %<>% left_join(select(country, country_id, country_text_id),
                      by = "country_id")

  full.df <- df %>%
        select(country_text_id, historical_date, v2ex_elechos,
               v2ex_elechog, v2lgbicam, v2lgello,
               v2lginello, v2lgelecup, v2lginelup, v2exapup, v2exapupap,
               v2exhoshog, v2expathhs, v2expathhg, v2exaphos, v2exaphogp,
               v2ex_hosw)

    # Backfill where there's only a -12-31 obs, and then frontfill
  input.df <-
        full.df %>%
        mutate(year = to_year(historical_date)) %>%
        # interpolate within the year
        left_join(select(utable, country_text_id, year, gap_idx),
                  by = c("country_text_id", "year")) %>%
        group_by(country_text_id, year) %>%
        arrange(historical_date) %>%
        mutate_all(interpolate) %>%
        # interpolate across years
        group_by(country_text_id, gap_idx) %>%
        arrange(historical_date) %>%
        mutate_all(locf) %>%
        ungroup %>%
        # Cleaning after interpolation
        mutate(v2lgello = ifelse(v2lgbicam == 0, 0, v2lgello),
               v2lginello = ifelse(v2lgbicam == 0, 0, v2lginello),
               v2lgelecup = ifelse(v2lgbicam < 2, NA, v2lgelecup),
               v2lginelup = ifelse(v2lgbicam < 2, NA, v2lginelup),
               v2exapup = ifelse(is.na(v2exapup), 0, v2exapup),
               v2exapupap = ifelse(is.na(v2exapupap), 0, v2exapupap),
               v2expathhg = ifelse(v2exhoshog == 1, NA, v2expathhg))

    # Run
    collectedInputs <- named_list(input.df)
    do.call(main, collectedInputs) %>%
      write_file(., OUTFILE, dir_create = TRUE)

  } else {
    # Call unit tests for main function and sub functions
    testthat::test_file("~/proj/mm-prep/tests/extra/test_elecoff.R")
  }
update_task_status(db = DB)
