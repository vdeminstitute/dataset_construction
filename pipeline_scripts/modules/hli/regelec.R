#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2xel_regelec.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))
#
# Functions
# --------------------------------------------------------------------------
combine_rownames <- function(nonc.ma, prob.v2elrgpwr) {
  combined_names <- union(rownames(nonc.ma), rownames(prob.v2elrgpwr))
  sprintf("Found %d combined country-dates", length(combined_names)) %>% info
  return(combined_names)
}

make_regelec <- function(nonc.ma, prob.v2elrgpwr, combined_names, utable) {
  nonc.ma <- stretch(nonc.ma, combined_names, preserve.na = FALSE, 
                     interpolate = T, utable = utable)
  prob.v2elrgpwr <- stretch(prob.v2elrgpwr, combined_names, utable = utable)

  stopifnot(identical(rownames(nonc.ma), rownames(prob.v2elrgpwr)))
  
  regelec <- prob.v2elrgpwr * nonc.ma[, "v2elsrgel"]

  no_reggov <- !is.na(nonc.ma[, "v2elreggov"]) & nonc.ma[, "v2elreggov"] == 0
  regelec[no_reggov, ] <- 0
  return(regelec)
}

main <- function(elsrgel, elreggov, prob.v2elrgpwr, utable, country, TASK_NAME) {
    # Prepare C-variable
    prob.v2elrgpwr %<>% mm_stretch_z_sample(., utable) %>% pnorm

    # Recode elsrgel
    elsrgel %<>% mutate(v2elsrgel = case_when(
        v2elsrgel %in% c(1, 3) ~ 1,
        v2elsrgel %in% c(2, 4) ~ 2,
        v2elsrgel == 0 ~ 0,
        v2elsrgel == 5 ~ 3))

    # Prepare non-C variables
    nonc.df <-
        full_join_vdem(elsrgel, elreggov) %>%
        add_country_cols(country) %>%
        clean_by_utable(utable)
    rownames(nonc.df) <- with(nonc.df, paste(country_text_id, historical_date))
    nonc.ma <- data.matrix(nonc.df[, c("v2elsrgel", "v2elreggov")])
    nonc.ma[, "v2elsrgel"] <- pnorm(nonc.ma[, "v2elsrgel"])

    # Calculate index
    combined_names <- combine_rownames(nonc.ma, prob.v2elrgpwr)
    make_regelec(nonc.ma, prob.v2elrgpwr, combined_names, utable) %>%
		hli_summary(., TASK_NAME)
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
    # Both A,C
    elsrgel <- objs[["v2elsrgel"]][["v2elsrgel"]]$cd
    elreggov <- objs[["v2elreggov"]][["v2elreggov"]]$cd
    # MM
    prob.v2elrgpwr <- objs[["v2elrgpwr"]]

    # Run
    collectedInputs <- named_list(elsrgel, elreggov, prob.v2elrgpwr, utable, 
                                  country, TASK_NAME)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
	    write_file(., OUTFILE, dir_create = TRUE)
    } else {
        testthat::test_file("~/proj/vdemds/module_unit_tests/hli/test_regelec.R") 
    }
