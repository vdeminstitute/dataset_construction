#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior files for v2x_api, v2x_mpi, v2x_polyarchy.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
# Calculate polyarchy by combining the additive and multiplicative polyarchy
calc_polyarchy <- function(prob.frassoc, prob.frefair, prob.freexp, extra, TASK_NAME) {
    
    # Multiplicative Polyarchy Index
    # -- every factor multiplies
    post.mpi <-
        extra[, "v2x_elecoff"] * prob.frefair * prob.frassoc * prob.freexp * extra[, "v2x_suffr"]
    # Additive Polyarchy Index
    # -- every factor adds
    post.api <-
        0.125 * extra[, "v2x_elecoff"] + 0.25 * prob.frefair + 0.25 * prob.frassoc +
            0.25 * prob.freexp + 0.125 * extra[, "v2x_suffr"]
    # (Combined) Polyarchy Index
    post.polyarchy <- 0.5 * post.mpi + 0.5 * post.api

    list(hli_summary(post.polyarchy, TASK_NAME), hli_summary(post.api, "v2x_api"),
        hli_summary(post.mpi, "v2x_mpi")) %>%
        setNames(c("v2x_polyarchy", "v2x_api", "v2x_mpi"))
}

main <- function(prob.frefair, prob.frassoc, prob.freexp, elecoff, suffr, 
    country, utable, TASK_NAME) {

    # Stretch BFA posterior samples
    prob.frefair <- bfa_stretch_z_sample(prob.frefair, utable)
    prob.frassoc <- bfa_stretch_z_sample(prob.frassoc, utable)
    prob.freexp <- bfa_stretch_z_sample(prob.freexp, utable)

    # Join elecoff and suffr
    extra.df <-
        clean_by_utable(
            full_join_vdem(
                df1=add_date_cols(add_country_cols(elecoff, country)),
                df2=add_date_cols(add_country_cols(suffr, country))),
            utable)

    # Create extra matrix
    extra <- data.matrix(extra.df[, c("v2x_elecoff", "v2x_suffr")])
    rownames(extra) <- with(extra.df, paste(country_text_id, historical_date))

    ll <- stretch_combined(
            named_list(prob.frassoc, prob.frefair, prob.freexp, extra), 
            utable) 
    
    info("Calculating v2x_api, v2x_mpi, v2x_polyarchy.")
    calc_polyarchy(
        ll$prob.frassoc,
        ll$prob.frefair,
        ll$prob.freexp,
        ll$extra,
        TASK_NAME)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Import
    country <- load_country()
    utable <- load_country_unit()
    objs <- find_dep_files(TASK_ID, db)
    elecoff <- objs[["v2x_elecoff"]][["v2x_elecoff"]]$cd 
    suffr <- objs[["v2x_suffr"]][["v2x_suffr"]]$cd 
    prob.frassoc <- objs[["v2x_frassoc_thick"]]
    prob.frefair <- objs[["v2xel_frefair"]]
    prob.freexp <- objs[["v2x_freexp_altinf"]]

    # Run
    collectedInputs <- named_list(prob.frefair, prob.frassoc, prob.freexp, elecoff, 
        suffr, country, utable, TASK_NAME)
    do.call(main, collectedInputs) %>% 
        write_file(., OUTFILE, dir_create = TRUE)
    } else {
        testthat::test_file("~/proj/vdemds/module_unit_tests/hli/test_polyarchy.R")
    }
