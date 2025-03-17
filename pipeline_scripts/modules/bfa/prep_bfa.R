# !/usr/bin/env Rscript

# ==========================================================================
# Prepares the input object for submit_bfa. It uses the z samples matrices
# from either mm jobs or binary indices. The output is a list of matrices 
# at combined reduced dates.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
# Prepare elecreg reg part
# -- only used if elecreg is used for interpolation
prep_elecreg <- function(df, country, utable) {

    df <- clean_by_utable(add_country_cols(df, country), utable)
    df[["country_id"]] <- NULL

	stopifnot(all(c("country_text_id", "v2x_elecreg") %in% names(df)))

	elecreg <- data.matrix(df$v2x_elecreg)
	rownames(elecreg) <- with(df, paste(country_text_id, historical_date))
	return(elecreg)
}

# Prepare each component to the BFA
prep_comps <- function(z, bfa_script, v, TASK_NAME, DRAWS) {

    # Get matrix and drop vignettes
	z <- load_matrix(z, drop.vignettes = TRUE)

	colnames(z) <- rep(v, ncol(z))

	if (grepl("perc", bfa_script)) {
		z <- scale(z)
	} else {
		z <- scale_matrix(z)
	}

    # Remove vignettes and sample draws
	out <- z[
        !is_vignette(rownames(z)),
        seq(1, by = as.integer(ncol(z) / DRAWS), length.out = DRAWS)]
	return(out)
}

# stretch the components to the combined reduced dates: the combined reduced
# dates are the union of the reduced dates from all components
stretch_comps <- function(m, cvar_names, utable, bfa_script, elecreg_cy = NULL) {
	# This stretching makes sure election specific (els) variables
	# are stretched to non-els dates as well. This is because els variables
	# For non-els we preserve NA rows which occur when coders didn't code 
	# that country-year.
	f <- partial(stretch, by = cvar_names, gaps = TRUE, rule_366 = FALSE,
		preserve.na = TRUE, utable = utable)

	if (grepl("perc", bfa_script)) {
		out <- f(m)
	} else {
		if (is.null(elecreg_cy)) {
			out <- f(m, interpolate = FALSE)
		} else {
			out <- f(m, interpolate = FALSE, elecreg_cy = elecreg_cy)
		}
	}
	return(out)
}

# for some indices that contains election-specific components, we need to stretch
# backwards if before the beginning of the coding period (1789) there was an election
# that made us hardcode elecreg to 1. 
stretch_els_towards_earlier_dates <- function(ll, qtable, elecreg_cy, elecreg_cd, 
					comps, utable, cvar_names, bfa_script, country) {
	els_vars <- 
		qtable %>% filter(name %in% comps, grepl("eltype", date_specific)) %$% name
	Map(function(m, nn) {
		if (!nn %in% els_vars)
			return(stretch_comps(m, cvar_names, utable, bfa_script))
		stretch_comps(m, cvar_names, utable, bfa_script, elecreg_cy) %>%
			front_filling_els_variables(., utable, elecreg_cy, elecreg_cd, country)
	}, m = ll, nn = names(ll))
}

prep_elec_regimes <- function(elecreg, cvar_names, utable) {
	full_elecreg_names <- union(cvar_names, rownames(elecreg))
	full_elecreg <- stretch(elecreg, full_elecreg_names, utable = utable)
	elec_regimes <- rownames(full_elecreg)[as.vector(!is.na(full_elecreg) & full_elecreg != 0)]
    
	return(elec_regimes)
}

# we do not want to stretch BFAs across years where lgbicam is 0, if that index
# contains variables that are coded only when lgbicam is > 0.
lgbicam_cleaning <- function(vars.ll, v2lgbicam) {
	cleaning_vars <- c("v2lgfunds", "v2lginvstp")
	v2lgbicam %<>% mutate(countrydate = paste(country_text_id, historical_date))
	vars.ll <- Map(function(v, n) {
		if (!n %in% cleaning_vars)
			return(v)
		info("Setting to lowest value where v2lgbicam is 0 for " %^% n)
		df <- full_join(data.frame(countrydate = rownames(v), bool = TRUE), 
			v2lgbicam, by = "countrydate") %>%
			mutate(date_character = get_date(countrydate),
				bool = ifelse(is.na(bool), FALSE, bool))
		df$historical_date[is.na(df$historical_date)] <- df$date_character[is.na(df$historical_date)]
		df %<>%	
			mutate(
				country_text_id = 
					ifelse(is.na(country_text_id), get_text_id(countrydate), 
						   country_text_id)) %>%
			group_by(country_text_id) %>%
			arrange(country_text_id, historical_date) %>%
			mutate(v2lgbicam = locf(v2lgbicam)) %>%
			# Fill towards earliest dates! Why?
			arrange(country_text_id, desc(historical_date)) %>%
			mutate(v2lgbicam = locf(v2lgbicam)) %>%
			ungroup %>%
			filter(countrydate %in% rownames(v)) %>% 
			untibble
		rownames(df) <- df$countrydate
		df <- df[rownames(v), ]
		bool <- df$v2lgbicam == 0
		min_value <- min(v, na.rm = TRUE)
		v[bool, ] <- min_value
		return(v)
	}, v = vars.ll, n = names(vars.ll))
	return(vars.ll)
}

# For BFAs, we only keep observations where at least 50% of the components are observed
# -- the rate argument controls this
drop_combined_missingness <- function(vars.ll, rate = 0.5) {

	missing.ma <- 
        do.call(rbind, lapply(vars.ll, function(ma) rowSums(is.na(ma)) == ncol(ma)))

    # Drop dates according to rate
	dropped_dates <- colnames(missing.ma)[colMeans(missing.ma) > rate]

	if (length(dropped_dates) > 0) {
        info(sprintf("Found %d country-dates with >50%% missingness", length(dropped_dates)))
	}
	return(dropped_dates)
}

# main
main <- function(objs, comps, frassoc, bfa_script, country, utable, TASK_NAME, qtable, DRAWS) {
	
    # Pre-allocate out container
    out <- list()
	# Pass full country_unit to output
	out$country_unit <- utable

    # elecreg or lgbicam are used for interpolation or cleaning
	comps <- comps[!comps %in% c("v2x_elecreg", "v2lgbicam")]
    # Pass components to output
	out$components <- comps

    # Pass the union of country_dates from each component to output
	out$country_dates <- Reduce(union, lapply(comps, function(v) {
        objs[[v]][[v]]$country_dates}))
	
	# Prepare components
	# -- scale components
    # -- cut off vignettes
	vars.ll <- setNames(lapply(comps, function(v) {
            prep_comps(
                z = objs[[v]][[v]]$post.sample$z,
                bfa_script = bfa_script,
                v = v,
                TASK_NAME = TASK_NAME,
                DRAWS = DRAWS)
            }), comps)

	# Stretch components to combined reduced dates
    # -- this is the union of the reduced dates from all components
	cvar_names <- sort(Reduce(union, lapply(vars.ll, rownames)))
	# Are there any election specific index components?
    elecregBool <- any(grepl("eltype", qtable$date_specific[qtable$name %in% comps]))
    # Apply stretch function conditional on there being any election specific components
	if (elecregBool) {
        info("Stretching election specific components to combined reduced dates")
		vars.ll <- stretch_els_towards_earlier_dates(vars.ll, qtable, 
			objs[["v2x_elecreg"]][["v2x_elecreg"]]$cy, objs[["v2x_elecreg"]][["v2x_elecreg"]]$cd,
			comps, utable, cvar_names, bfa_script, country)
	} else {
		vars.ll <- lapply(vars.ll, function(m) {
            info("Stretching components to reduced combined dates")
            stretch_comps(m, cvar_names, utable, bfa_script)
            })
        }

	# for v2xnp_pres set legislative variables to missing where v2lgbicam is 0
	if (TASK_NAME == "v2xnp_pres") {
		v2lgbicam <- left_join(objs[["v2lgbicam"]][["v2lgbicam"]]$cd,
			select(country, country_id, country_text_id), by = "country_id")
		vars.ll <- lgbicam_cleaning(vars.ll, v2lgbicam)
	}

	# Identify rows where more than 50 percent of components are missing.
	# -- index will be set to missing for those country-dates
    # -- keep track of these dates, so that we preserve them as NA post calculation
	out$dropped_dates <- drop_combined_missingness(vars.ll, rate = 0.5)
	
    # Define the z.samples object
    out$z.samples <- lapply(vars.ll, function(m) {
        m[!rownames(m) %in% out$dropped_dates,, drop = FALSE] })
	cvar_names <- setdiff(cvar_names, out$dropped_dates)
	stopifnot(do.call(all_identical, lapply(vars.ll, rownames)))

    # n_obs is total output length of `xi`, i.e. total number of unique country-dates.
	out$n_obs <- length(cvar_names)
    info(sprintf("Total number of country-dates: %d", out$n_obs))

    # Define script to use
	out$script <- file.path("jags", paste0(bfa_script, ".jag"))

    # For frassoc: 
	if (frassoc) {
        # Expand elecreg to match the rownames from our dependencies.
        # -- 1) country-dates missing from elecreg and
        # -- 2) front-fill with gaps to create a full elecreg ts.
		out$elec_regimes <-
            prep_elec_regimes(
                prep_elecreg(
                    objs[["v2x_elecreg"]][["v2x_elecreg"]]$cd,
                    country,
                    utable),
                cvar_names,
                utable)
	}

	if (TASK_NAME == "v2xel_frefair") {
		out$elecreg_cy <- objs[["v2x_elecreg"]][["v2x_elecreg"]]$cy
	}

	# Parameters for model can differ by BFA:
    # -- MCMC = for how many iterations in total?
    # -- THIN = keep every THINth iteration (e.g. if THIN=10, keep every 10th).
	# These indices have many components and the objects become too large
	# They take very long to compute and use a lot of memory.
	if (TASK_NAME %in% c("v2x_rule", "v2x_neopat")) {
		out$MCMC <- 3000L
		out$THIN <- 500L
	} else {
		out$MCMC <- 12000L
		out$THIN <- 2000L
	}

	return(out)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
    db <- pg_connect()
	get_globals()

	# Imports
	qtable <- load_qtable()
	country <- load_country()
	utable <- load_country_unit()
	frassoc <- TASK_NAME == "v2x_frassoc_thick"

    # Default iteration
    DRAWS <- 600L

    # Which .jags script to use?
	if (qtable$bfa_perc[qtable$name == TASK_NAME]) {
		bfa_script <- "bfa_perc"
	} else {
		bfa_script <- "bfa"
	}
	stopifnot(bfa_script != "", !is.null(bfa_script))

	objs <- find_dep_files(TASK_ID, db)
	comps <- unlist(strsplit(qtable$components[qtable$name == TASK_NAME], " "))
    qtable <- subset(qtable, name %in% c(TASK_NAME, comps))

	# Run
	collectedInput <- named_list(objs, comps, frassoc, bfa_script, country, utable, TASK_NAME, qtable, DRAWS)
	setNames(list(do.call(main, collectedInput)), TASK_NAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)

	} else {
		testthat::test_file("~/proj/vdemds/module_unit_tests/bfa/test_prep_bfa.R")
	}
