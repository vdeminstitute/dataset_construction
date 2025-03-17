#!/usr/bin/env Rscript

# ==========================================================================
# Clean frefair thin posterior and by setting to zero when there's no
# electoral regime according to v2x_elecreg.
#
# We also clean the frefair index scores by setting to zero where 
# v2x_elecreg is zero, and then aggregate to country-year using 
# day-weighted mean.
#
# Note that we also set v2xel_frefair to 0 when v2x_elecreg is 0 
# and there was no v2xel_frefair score existing before
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Functions
# --------------------------------------------------------------------------
nan_fix <- function(df) {
    df[] <- lapply(df, function(v) { 
        if (!is.numeric(v))
            return(v)
        is.na(v) <- is.nan(v)
        return(v)
    })
    return(df)
}

clean_frefair <- function(input.df, lev, country, elecreg, utable, TASK_NAME) {
    lev <- match.arg(lev, c("cd", "cy"))

    df <- input.df[[lev]] |> add_country_cols(country) |> add_date_cols()
    
    elecreg <- elecreg[[lev]] |> 
        add_country_cols(country) |> 
        add_date_cols() |> 
        clean_by_utable(utable)

    df %<>%
        full_join_vdem(elecreg) |> 
		interpolate_components(
			df = _, 
			cols = "v2x_elecreg", 
			utable = utable,
			keep_nan = FALSE, 
			coder_level = FALSE)

    cols <- grep("frefair", colnames(df), value = TRUE)

	# Clean only when v2x_elecreg is not missing
    df[!is.na(df$v2x_elecreg) & df$v2x_elecreg == 0L, cols] <- 0

	f <- function(x) !is.na(x) 
	df <-
        select(df, -v2x_elecreg) %>%
        filter(if_any(one_of(TASK_NAME), f))
		
	return(df)
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
    frefair.ma <- stretch(frefair.ma, combined_names, utable = utable)
    elecreg.ma <- stretch(elecreg.ma, combined_names, utable = utable)

    stopifnot(identical(rownames(frefair.ma), rownames(elecreg.ma)))

    nonelec_regimes <- !is.na(elecreg.ma[, 1]) & elecreg.ma[, 1] == 0
    info(sprintf("Setting %d rows to 0", sum(nonelec_regimes)))
    frefair.ma[nonelec_regimes, ] <- 0
    return(frefair.ma)
}

main <- function(input.df, country, elecreg, utable, frefair.ma, TASK_NAME) {

    cd <-
        clean_frefair(input.df, lev = "cd", country, elecreg, utable, TASK_NAME) |> 
        select(-country_id)
    cy <- cy.day_mean(cd, historical_date, country_text_id, mc.cores = 1)
    posterior <- clean_posterior(elecreg, country, utable, frefair.ma)
    
    return(list(cd = cd, cy = cy, thin_post = posterior))
    
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
    input.df <- objs[[TASK_NAME]][[TASK_NAME]]

    # NaN fix
    is.na(input.df$thin_post) <- is.nan(input.df$thin_post)
    input.df$cd <- nan_fix(input.df$cd)
    input.df$cy <- nan_fix(input.df$cy)

    frefair.ma <- clean_by_utable(input.df$thin_post, utable)
    elecreg <- objs[["v2x_elecreg"]][["v2x_elecreg"]]

    # Run
    collectedInputs <- named_list(input.df, country, elecreg, utable, frefair.ma, TASK_NAME)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/bfa/test_clean_frefair.R")
}
