#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_EDcomp_thick
# ==========================================================================

options(warn = 2)

suppressMessages(library(docopt))
suppressMessages(library(dplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "v2x_EDcomp_thick")

#
# Functions
# --------------------------------------------------------------------------
calc_edcomp <- function(matrix_list) {
    stopifnot(c("extra", "prob.frassoc", "prob.frefair") %in% names(matrix_list),
        is.list(matrix_list),
        length(matrix_list) == 3)

    extra <- matrix_list[["extra"]]
    return(.125 * matrix_list[["prob.frassoc"]] + .125 * extra[, "v2x_suffr"] + 
        .125 * matrix_list[["prob.frefair"]] +
        .125 * extra[, "v2x_elecoff"] + .5 * matrix_list[["prob.frassoc"]] * 
        extra[, "v2x_suffr"] * matrix_list[["prob.frefair"]] * extra[, "v2x_elecoff"])
}

prep_variable <- function(obj, utable, country) {
    obj %>%
        add_country_cols(country) %>%
        add_date_cols %>%
        clean_by_utable(utable)
}

main <- function(elecoff, suffr, prob.frassoc, prob.frefair, utable, 
                 country, VARNAME) {
 
    # Prepare matrices for non-MM / non-BFA variables
    elecoff %<>% prep_variable(., utable, country)
    suffr %<>% prep_variable(., utable, country)
    extra.df <-
        full_join_vdem(elecoff, suffr) %>%
        select(country_text_id, historical_date, v2x_elecoff, v2x_suffr)
    extra <- select(extra.df, v2x_elecoff, v2x_suffr) %>% data.matrix
    rownames(extra) <- with(extra.df, paste(country_text_id, historical_date))

    # Prepare BFAs
    prob.frassoc %<>% bfa_stretch_z_sample(., utable)
    prob.frefair %<>% bfa_stretch_z_sample(., utable)

    # Stretch, combine, calculate
    stretch_combined(named_list(extra, prob.frassoc, prob.frefair), utable) %>%
        calc_edcomp(.) %>%
		hli_summary(., VARNAME)
}


# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    country <- load_country()
    utable <- load_country_unit()
    objs <- find_dep_files(TASK_ID, DB)
    elecoff <- objs[["v2x_elecoff"]][["v2x_elecoff"]]$cd
    suffr <- objs[["v2x_suffr"]][["v2x_suffr"]]$cd 
    prob.frassoc <- objs[["v2x_frassoc_thick"]]
    prob.frefair <- objs[["v2xel_frefair"]]

    # Run
    collectedInputs <- named_list(elecoff, suffr, prob.frassoc, prob.frefair, utable, 
                                  country, VARNAME)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
} else {
    # Call unit tests for main function and sub functions
    testthat::test_file("~/proj/mm-prep/tests/tests_EDcomp.R")
}
update_task_status(db = DB)
