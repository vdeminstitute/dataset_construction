#!/usr/bin/env Rscript

# ==========================================================================
# Clean frefair thin posterior and by setting to zero when there's no
# electoral regime according to v2x_elecreg. This is only needed for
# polyarchy.
#
# We also clean the frefair index scores by setting to zero where 
# v2x_elecreg is zero, and then aggregate to country-year using 
# day-weighted mean.
# ==========================================================================

options(warn = 2)
suppressMessages(library(vutils))
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "clean_frefair")

# Functions
# --------------------------------------------------------------------------
clean_frefair <- function(input.df, lev, country, elecreg, utable, VARNAME) {
    lev <- match.arg(lev, c("cd", "cy"))

    df <- input.df[[lev]] %>% add_country_cols(country) %>% add_date_cols
    
    elecreg <- elecreg[[lev]] %>%
        add_country_cols(country) %>%
        add_date_cols %>%
        clean_by_utable(utable)

    df %<>%
        full_join_vdem(elecreg) %>%
        interpolate_vdem_style(col = "v2x_elecreg", utable)
    cols <- grep("frefair", colnames(df), value = TRUE)
    df[!is.na(df$fill_col) & df$fill_col == 0, cols] <- 0
    df %<>%
        select(-fill_col, -v2x_elecreg) %>%
        filter(!is.na(get(VARNAME)))
}

clean_posterior <- function(elecreg, country, utable, frefair.ma) {
    elecreg <-
        elecreg$cd %>%
        add_country_cols(country) %>%
        clean_by_utable(utable) %>%
        select(country_text_id, historical_date, v2x_elecreg)
    elecreg.ma <- data.matrix(elecreg[, "v2x_elecreg"])
    rownames(elecreg.ma) <- with(elecreg, paste(country_text_id, historical_date))
    combined_names <- union(rownames(frefair.ma), rownames(elecreg.ma))

    # Match and stretch rownames for each matrix
    frefair.ma <- stretch(frefair.ma, combined_names, interpolate = T,
                          utable = utable)
    elecreg.ma <- stretch(elecreg.ma, combined_names, utable = utable)

    stopifnot(identical(rownames(frefair.ma), rownames(elecreg.ma)))

    nonelec_regimes <- !is.na(elecreg.ma[, 1]) & elecreg.ma[, 1] == 0
    sprintf("Setting %d rows to 0", sum(nonelec_regimes)) %>% info
    frefair.ma[nonelec_regimes, ] <- 0
    return(frefair.ma)
}

main <- function(input.df, country, elecreg, utable, frefair.ma, VARNAME) {
    cd <- clean_frefair(input.df, lev = "cd", country, elecreg, utable, VARNAME) %>%
        select(-country_id)
    cy <- cy.day_mean(cd, historical_date, country_text_id, mc.cores = 1)
    posterior <- clean_posterior(elecreg, country, utable, frefair.ma)
    return(list(cd = cd, cy = cy, thin_post = posterior))
}


nan_fix <- function(df) {
    df[] <- lapply(df, function(v) { 
        if (!is.numeric(v))
            return(v)
        is.na(v) <- is.nan(v)
        return(v)
    })
    return(df)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    country <- load_country()
    utable <- load_country_unit()
    objs <- find_dep_files(TASK_ID, DB)
    input.df <- objs[[VARNAME]][[VARNAME]]

    # NaN fix
    is.na(input.df$thin_post) <- is.nan(input.df$thin_post)
    input.df$cd <- nan_fix(input.df$cd)
    input.df$cy <- nan_fix(input.df$cy)

    frefair.ma <- input.df$thin_post %>%
        clean_by_utable(utable)
    elecreg <- objs[["v2x_elecreg"]][["v2x_elecreg"]]

    # Run
    collectedInputs <- named_list(input.df, country, elecreg, utable, frefair.ma, VARNAME)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
} else {
    testthat::test_file("~/proj/mm-prep/tests/bfa/test_clean_frefair.R")
}
update_task_status(db = DB)