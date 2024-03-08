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
library(psych)
library(vutils)

set_env(MODULE_NAME = "party")

#
# Functions
# --------------------------------------------------------------------------
cleaning <- function(vdem) {
  # Exclude legislative cohesion for years when legislatures do not exist
  vdem$v2pscohesv <- ifelse(is.na(vdem$v2lgello), NA, vdem$v2pscohesv)
  vdem$v2pscohesv_codelow68 <- ifelse(is.na(vdem$v2lgello), NA,
                                      vdem$v2pscohesv_codelow68)
  vdem$v2pscohesv_codehigh68 <- ifelse(is.na(vdem$v2lgello), NA,
                                      vdem$v2pscohesv_codehigh68)
  return(vdem)
}

scaling <- function(vdem) {
  # Scalling includes code_low and code_high versions of vars
  # so bounds are in the same scale of index
  orgs <- c(vdem$v2psorgs, vdem$v2psorgs_codelow68, vdem$v2psorgs_codehigh68)
  brch <- c(vdem$v2psprbrch, vdem$v2psprbrch_codelow68, vdem$v2psprbrch_codehigh68)
  plat <- c(vdem$v2psplats, vdem$v2psplats_codelow68, vdem$v2psplats_codehigh68)
  lnks <- c(vdem$v2psprlnks, vdem$v2psprlnks_codelow68, vdem$v2psprlnks_codehigh68)
  cohs <- c(vdem$v2pscohesv, vdem$v2pscohesv_codelow68, vdem$v2pscohesv_codehigh68)

  orgs_s <- scale(orgs)
  brch_s <- scale(brch)
  plat_s <- scale(plat)
  lnks_s <- scale(lnks)
  cohs_s <- scale(cohs)

  vars_s <- cbind(orgs_s, brch_s, plat_s, lnks_s, cohs_s)
  # Check if all std normal
  describe(vars_s) #Yay!
  return(list(orgs_s = orgs_s, brch_s = brch_s, plat_s = plat_s,
    lnks_s = lnks_s, cohs_s = cohs_s))
}



gen_index <- function(index_list, vdem) {

    # Separate point-estimates from bounds
    fn <- function(v) split(v, sort(seq_along(v) %% 3))
    ll <- fn(index_list[["orgs_s"]])
  psorgs <- data.frame(v2psorgs = ll[[1]],
                       v2psorgs_codelow = ll[[2]],
                       v2psorgs_codehigh = ll[[3]])

  ll <- fn(index_list[["brch_s"]])
  psbrch <- data.frame(v2psprbrch = ll[[1]],
                       v2psprbrch_codelow = ll[[2]],
                       v2psprbrch_codehigh = ll[[3]])

  ll <- fn(index_list[["plat_s"]])
  psplats <- data.frame(v2psplats = ll[[1]],
                       v2psplats_codelow = ll[[2]],
                       v2psplats_codehigh = ll[[3]])

  ll <- fn(index_list[["lnks_s"]])
  psprlnks <- data.frame(v2psprlnks = ll[[1]],
                       v2psprlnks_codelow = ll[[2]],
                       v2psprlnks_codehigh = ll[[3]])

  ll <- fn(index_list[["cohs_s"]])
  pscohesv <- data.frame(v2pscohesv = ll[[1]],
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
  pi_index <- data.frame(country_text_id = vdem$country_text_id,
                        year = vdem$year,
                        v2xps_party = pi_index,
                        v2xps_party_codelow = pi_index_codelow,
                        v2xps_party_codehigh = pi_index_codehigh,
                        stringsAsFactors = FALSE)
    return(list(cy = pi_index))
}

main <- function(vdem) {
    cleaning(vdem) %>%
        scaling(.) %>%
        gen_index(., vdem)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    country <- load_country()
    objs <- find_dep_files(TASK_ID, DB)

    vdem <- lapply(names(objs), function(v) {
            add_country_cols(objs[[v]][[v]]$cy, country)
        }) %>%
        Reduce(full_join_vdem, .)

    # Run
    setNames(list(main(vdem)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)

} else {
    testthat::test_file("~/proj/mm-prep/tests/extra/test_party.R")
}
update_task_status(db = DB)
