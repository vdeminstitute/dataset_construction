#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior files for v2x_api, v2x_mpi, v2x_polyarchy.
# ==========================================================================

options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "v2x_polyarchy")

#
# Functions
# --------------------------------------------------------------------------

calc_polyarchy <- function(prob.frassoc, prob.frefair, prob.freexp, extra, VARNAME) {
    # MPI
    post.mpi <- extra[, "v2x_elecoff"] * prob.frefair * prob.frassoc * prob.freexp * extra[, "v2x_suffr"]
    # API
    post.api <- 0.125 * extra[, "v2x_elecoff"] + 0.25 * prob.frefair + 0.25 * prob.frassoc +
        0.25 * prob.freexp + 0.125 * extra[, "v2x_suffr"]
    # Polyarchy
    post.polyarchy <- 0.5 * post.mpi + 0.5 * post.api

    list(hli_summary(post.polyarchy, VARNAME), hli_summary(post.api, "v2x_api"),
        hli_summary(post.mpi, "v2x_mpi")) %>%
        setNames(c("v2x_polyarchy", "v2x_api", "v2x_mpi"))
}

main <- function(prob.frefair, prob.frassoc, prob.freexp, elecoff, suffr, 
                 country, utable, VARNAME) {
    
    info("Stretching BFA components...")
    prob.frefair %<>% bfa_stretch_z_sample(., utable)
    prob.frassoc %<>% bfa_stretch_z_sample(., utable)
    prob.freexp %<>% bfa_stretch_z_sample(., utable)


    info("Creating matrices for v2x_elecoff and v2x_suffr...")
    elecoff %<>% add_country_cols(country) %>% add_date_cols
    suffr %<>% add_country_cols(country) %>% add_date_cols
    extra.df <- full_join_vdem(elecoff, suffr) %>%
        clean_by_utable(utable)
    extra <- data.matrix(extra.df[, c("v2x_elecoff", "v2x_suffr")])
    rownames(extra) <- with(extra.df, paste(country_text_id, historical_date))

    info("Stretching components to combined dates...")
    ll <- stretch_combined(named_list(prob.frassoc, prob.frefair, prob.freexp, extra), 
                           utable) 
    info("Calculate v2x_api, v2x_mpi, v2x_polyarchy...")
    calc_polyarchy(ll$prob.frassoc, ll$prob.frefair, ll$prob.freexp, ll$extra, VARNAME)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Import
    country <- load_country()
    utable <- load_country_unit()
    objs <- find_dep_files(TASK_ID, DB)
    elecoff <- objs[["v2x_elecoff"]][["v2x_elecoff"]]$cd 
    suffr <- objs[["v2x_suffr"]][["v2x_suffr"]]$cd 
    prob.frassoc <- objs[["v2x_frassoc_thick"]]
    prob.frefair <- objs[["v2xel_frefair"]]
    prob.freexp <- objs[["v2x_freexp_altinf"]]

    # Run
    collectedInputs <- named_list(prob.frefair, prob.frassoc, prob.freexp, elecoff, 
        suffr, country, utable, VARNAME)
    do.call(main, collectedInputs) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    } else {
        testthat::test_file("~/proj/mm-prep/tests/hli/test_polyarchy.R")
    }
update_task_status(db = DB)
