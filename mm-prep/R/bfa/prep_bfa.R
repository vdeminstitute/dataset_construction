# !/usr/bin/env Rscript

suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "prep_bfa")

# BFA sample draws
DRAWS <- 600L

#
# Functions
# --------------------------------------------------------------------------
prep_elecreg <- function(df, country, utable) {
	df %<>%
		add_country_cols(country) %>%
		clean_by_utable(utable) %>%
		select(-country_id)

	stopifnot(all(c("country_text_id", "v2x_elecreg") %in% names(df)))

	elecreg <- data.matrix(df$v2x_elecreg)
	rownames(elecreg) <- with(df, paste(country_text_id, historical_date))
	return(elecreg)
}

prep_comps <- function(z, bfa_script, v, VARNAME) {
	m <- load_matrix(z)
	colnames(m) <- rep(v, ncol(m))

	if (grepl("perc", bfa_script)) {
		m <- scale(m)
	} else {
		m <- scale_matrix(m)
	}

	out <- m[!is_vignette(rownames(m)), seq(1, by = as.integer(ncol(m) / DRAWS), length.out = DRAWS)]
	return(out)
}

stretch_comps <- function(m, cvar_names, utable, bfa_script, elecreg_cy = NULL) {
	# This stretching makes sure election specific (els) variables
	# are stretched to non-els dates as well. This is because els variables
	# DO NOT have any rows with missingness added for empty country-years.
	# For non-els we preserve NA rows which occur when coders didn't code 
	# that country-year.
	f <- partial(stretch, by = cvar_names, gaps = TRUE, rule_366 = FALSE,
		preserve.na = TRUE, utable = utable)

	if (grepl("perc", bfa_script)) {
		out <- f(m)
	} else {
		if (is.null(elecreg_cy)) {
			out <- f(m, interpolate = TRUE)
		} else {
			out <- f(m, interpolate = TRUE, elecreg_cy = elecreg_cy)
		}
	}
	return(out)
}

make_dropped <- function(vars.ll) {
	missing.ma <- lapply(vars.ll, function(ma) rowSums(is.na(ma)) == ncol(ma)) %>%
	    do.call(rbind, .)
	info(dim(missing.ma))
	dropped_dates <- colnames(missing.ma)[colMeans(missing.ma) > .5]

	if (length(dropped_dates) > 0) {
	    sprintf("Found %d country-dates with >50%% missingness",
	            length(dropped_dates)) %>% info
	}
	return(dropped_dates)
}

prep_elec_regimes <- function(elecreg, cvar_names, utable) {
	full_elecreg_names <- union(cvar_names, rownames(elecreg))
	full_elecreg <- stretch(elecreg, full_elecreg_names, utable = utable)
	elec_regimes <- rownames(full_elecreg)[as.vector(!is.na(full_elecreg) & full_elecreg != 0)]
	return(elec_regimes)
}

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


main <- function(objs, comps, frassoc, bfa_script, country, utable, VARNAME, qtable) {
	out <- list()
	comps <- comps[!comps %in% c("v2x_elecreg", "v2lgbicam")]

	# Pass full country-dates and country_unit to output
	out$country_unit <- utable
	out$components <- comps
	out$country_dates <- Reduce(union, lapply(comps, function(v) {
  		  	objs[[v]][[v]]$country_dates}))
	
	# Prepare components
	# Load and scale components, cut off vignettes
	vars.ll <- lapply(comps, function(v) {
	        objs[[v]][[v]]$post.sample$z %>%
	        	prep_comps(bfa_script, v, VARNAME)
	    }) %>% setNames(comps)

	# Stretch components to combined reduced dates
	cvar_names <- Reduce(union, lapply(vars.ll, rownames)) %>% sort
	
	# Are there any election specific index components?
	if (qtable %>% filter(name %in% comps) %$% date_specific %>% grepl("eltype", .) %>% any) {
		vars.ll <- stretch_els_towards_earlier_dates(vars.ll, qtable, 
			objs[["v2x_elecreg"]][["v2x_elecreg"]]$cy, objs[["v2x_elecreg"]][["v2x_elecreg"]]$cd,
			comps, utable, cvar_names, bfa_script, country)
	} else {
		vars.ll <- lapply(vars.ll, function(m) stretch_comps(m, cvar_names, utable, bfa_script))
	}



	# for v2xnp_pres set legislative variables to missing where v2lgbicam is 0
	if (VARNAME == "v2xnp_pres") {
		v2lgbicam <- left_join(objs[["v2lgbicam"]][["v2lgbicam"]]$cd,
			select(country, country_id, country_text_id), by = "country_id")
		vars.ll <- lgbicam_cleaning(vars.ll, v2lgbicam)
	}

	# Identify rows where more than 50 percent of components are missing.
	# The index will be set to missing for those country-dates.
	# We'll need this after running the model to set the rows we want
	# missing (50% of input vars were missing) to NA so that we don't
	# stretch over those dates
	out$dropped_dates <- make_dropped(vars.ll)
	out$z.samples <- lapply(vars.ll, function(m) m[!rownames(m) %in% out$dropped_dates,, drop = F])
	cvar_names <- setdiff(cvar_names, out$dropped_dates)

	stopifnot(do.call(all_identical, lapply(vars.ll, rownames)))

	# Initial values. n_obs = total output length of `xi`, i.e. total
	# number of unique country-dates.
	out$n_obs <- length(cvar_names)
	out$script <- file.path("jags", paste0(bfa_script, ".jag"))

	if (frassoc) {
		out$elec_regimes <- objs[["v2x_elecreg"]][["v2x_elecreg"]]$cd %>%
	        prep_elecreg(country, utable) %>%
	        # We need to expand elecreg to match the rownames from our dep
			# vars. Find first country-dates missing from elecreg and then
			# front-fill with gaps to create a full elecreg ts.
			prep_elec_regimes(cvar_names, utable)
	}

	if (VARNAME == "v2xel_frefair") {
		out$elecreg_cy <- objs[["v2x_elecreg"]][["v2x_elecreg"]]$cy
	}

	out$MCMC <- 12000L
	out$THIN <- 2000L

	return(out)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
	# Global variables
	get_globals()

	# Imports
	qtable <- load_qtable()
	country <- load_country()
	utable <- load_country_unit()
	frassoc <- VARNAME == "v2x_frassoc_thick"
	
	if (qtable %>% filter(name == VARNAME) %$% bfa_perc) {
		bfa_script <- "bfa_perc"
	} else {
		bfa_script <- "bfa"
	}
	stopifnot(bfa_script != "", !is.null(bfa_script))

	objs <- find_dep_files(TASK_ID, DB)
	comps <- qtable %>% filter(name == VARNAME) %>% pull(components) %>%
	    strsplit(., " ") %>% unlist

	# Run
	collectedInput <- named_list(objs, comps, frassoc, bfa_script, country, utable, VARNAME, qtable)
	setNames(list(do.call(main, collectedInput)), VARNAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)

	} else {
		testthat::test_file("~/proj/mm-prep/tests/bfa/test_prep_bfa.R")
	}
update_task_status(db = DB)
