#!/usr/bin/env Rscript

# ==========================================================================
# Party Institutionalization Index
### Authors ###
# Fernando Bizzarro
# Harvard University
# fbizzarroneto@g.harvard.edu

# Allen Hicken
# University of Michigan

# Darin Self
# Cornell university

### Summary ###

# This code creates the V-Dem Party Institutionalization Index.
# The code is a simple additive version of the following variables:
# v2psorgs, v2psprbrch, v2psprlnks, v2psplats, v2pscohesv.
# The sum normalized to 0 to 1.

# For more information on the index, please refer to:
# Bizzarro, Fernando. Hicken, Allen. Self, Darin. The V-Dem Party Institutionalization Index: A new global indicator (1900-2015).
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
cleanComponents <- function(dat) {
    # Exclude legislative cohesion for years when legislatures do not exist

    dat$v2pscohesv <- ifelse(is.na(dat$v2lgello), NA_real_, dat$v2pscohesv)

    dat$v2pscohesv_codelow68 <- ifelse(is.na(dat$v2lgello), NA_real_,
                                        dat$v2pscohesv_codelow68)
    
    dat$v2pscohesv_codehigh68 <- ifelse(is.na(dat$v2lgello), NA_real_,
                                        dat$v2pscohesv_codehigh68)
    
    return(dat)
}

scaleComponents <- function(dat) {
    # Scaling includes code_low and code_high versions of vars
    # so bounds are in the same scale of index

    orgs <- c(dat$v2psorgs, dat$v2psorgs_codelow68, dat$v2psorgs_codehigh68)
    brch <- c(dat$v2psprbrch, dat$v2psprbrch_codelow68, dat$v2psprbrch_codehigh68)
    plat <- c(dat$v2psplats, dat$v2psplats_codelow68, dat$v2psplats_codehigh68)
    lnks <- c(dat$v2psprlnks, dat$v2psprlnks_codelow68, dat$v2psprlnks_codehigh68)
    cohs <- c(dat$v2pscohesv, dat$v2pscohesv_codelow68, dat$v2pscohesv_codehigh68)

    orgs_s <- scale(orgs)
    brch_s <- scale(brch)
    plat_s <- scale(plat)
    lnks_s <- scale(lnks)
    cohs_s <- scale(cohs)

    vars_s <- cbind(orgs_s, brch_s, plat_s, lnks_s, cohs_s)

    return(list(orgs_s = orgs_s, brch_s = brch_s, plat_s = plat_s,
        lnks_s = lnks_s, cohs_s = cohs_s))
}

generateIndex <- function(index_list, dat) {

    # Separate point-estimates from bounds
    fn <- function(v) {
        split(v, sort(seq_along(v) %% 3))
    }

    ll <- fn(index_list[["orgs_s"]])
    psorgs <- data.frame(
        v2psorgs = ll[[1]],
        v2psorgs_codelow = ll[[2]],
        v2psorgs_codehigh = ll[[3]])

    ll <- fn(index_list[["brch_s"]])
    psbrch <- data.frame(
        v2psprbrch = ll[[1]],
        v2psprbrch_codelow = ll[[2]],
        v2psprbrch_codehigh = ll[[3]])

    ll <- fn(index_list[["plat_s"]])
    psplats <- data.frame(
        v2psplats = ll[[1]],
        v2psplats_codelow = ll[[2]],
        v2psplats_codehigh = ll[[3]])

    ll <- fn(index_list[["lnks_s"]])
    psprlnks <- data.frame(
        v2psprlnks = ll[[1]],
        v2psprlnks_codelow = ll[[2]],
        v2psprlnks_codehigh = ll[[3]])

    ll <- fn(index_list[["cohs_s"]])
    pscohesv <- data.frame(
        v2pscohesv = ll[[1]],
        v2pscohesv_codelow = ll[[2]],
        v2pscohesv_codehigh = ll[[3]])


    # Adding up Point estimates
    pi_index <- psorgs$v2psorgs + psbrch$v2psprbrch + psplats$v2psplats +
        psprlnks$v2psprlnks + pscohesv$v2pscohesv

    # Adding up Bounds
    pi_index_codelow <- psorgs$v2psorgs_codelow + psbrch$v2psprbrch_codelow +
        psplats$v2psplats_codelow + psprlnks$v2psprlnks_codelow +
        pscohesv$v2pscohesv_codelow

    pi_index_codehigh <- psorgs$v2psorgs_codehigh + psbrch$v2psprbrch_codehigh +
        psplats$v2psplats_codehigh + psprlnks$v2psprlnks_codehigh +
        pscohesv$v2pscohesv_codehigh

    # Normalizing using the CDF
    fun <- ecdf(pi_index)
    pi_index <- fun(pi_index)
    pi_index_codelow <- fun(pi_index_codelow)
    pi_index_codehigh <- fun(pi_index_codehigh)

    # Merge and Store new index
    pi_index <- data.frame(
        country_text_id = dat$country_text_id,
        year = dat$year,
        v2xps_party = pi_index,
        v2xps_party_codelow = pi_index_codelow,
        v2xps_party_codehigh = pi_index_codehigh,
        stringsAsFactors = FALSE)
        return(list(cy = pi_index))
    }

main <- function(dat) {
    generateIndex(
        index_list = scaleComponents(cleanComponents(dat)),
        dat = dat)
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
    objs <- find_dep_files(TASK_ID, db)

    dat <- Reduce(full_join_vdem, lapply(names(objs), function(v) {
            add_country_cols(objs[[v]][[v]]$cy, country)
        }))

    # Run
    setNames(list(main(dat)), TASK_NAME) |>
        write_file(o=_, OUTFILE, dir_create = TRUE)

} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/extra/test_party.R")
}
