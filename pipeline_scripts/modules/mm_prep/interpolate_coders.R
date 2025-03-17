#!/usr/bin/env Rscript

# ==========================================================================
# The purpose of this script is to go from a data.frame with all expert
# ratings in one vector to expert ratings spread across one column per coder_id.
# The script operates per variable.
# 
# Ratings that are identified as lateral, and ratings submitted by coders that
# only give lateral scores are removed before interpolating scores.
# 
# The function to_wide_form allows for missing values, but only if they occur
# on January first (a consequence of adding January first). All other NAs are 
# seen as errors. The NAs from January first are removed in the reduce script.
#
# The interpolation then happens in the following situations: 
# 1. Within a single year, only forwards
# 2. Between year t and t+1 if: t+1 is not fully NA
#
# The order of operations in the scripts is:
# 1. Remove experts that only provide lateral scores
# 2. If there are lateral coders for the question:
#   2.1 Identify the lateral scores
#   2.2 Remove the lateral scores added from january first script;
#   the residual observations are defined as lateral vignettes.
# 3. The lateral scores are removed from the "real scores". The scores are
#   then put into wide format with coder as columns, which produce NAs.
# 4. The wide scores are interpolated according to the list above. 
#
# ==========================================================================

suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------

# Experts that only give lateral scores are removed
remove_lateral_only_coders <- function(df, coder_table) {

    lateral_only <- tapply(coder_table$lateral, coder_table$coder_id, all)
    lateral_only <- as.numeric(names(lateral_only)[lateral_only])
    stopifnot(!anyNA(lateral_only))

    out <- df[!df[["coder_id"]] %in% lateral_only, , drop = FALSE] 
    
    return(out)
}

# Defines which observations that are lateral
define_lateral <- function(df, coder_table) {

    stopifnot(is.logical(coder_table$lateral))
    
    lateral_coders <- coder_table[
        with(coder_table, lateral & !is.na(lateral)),
        c("coder_id", "country_id")]

    df <- dplyr::semi_join(
        x = df,
        y = lateral_coders,
        by = c("coder_id", "country_id"))

    if (nrow(df) == 0) {
        info(sprintf("No lateral coders found for %s",
            unique(coder_table$question_name)))
    }
    
    return(df)
}

# splits a long vector into a data.frame with coder_id as column and rows as
# country_text_id and historical_date.
to_wide_form <- function(df, ctable) {

    df %<>%
        mutate(country_text_id = to_ctext_ids(country_id, ctable)) %>%
        select(country_text_id, historical_date, coder_id, code,
            confidence)

    # Negative score on code is not allowed.
    stopifnot(`df contains negative values for code` = !any(df$code < 0, na.rm = TRUE))
    # NAs on code/confidence. This is a fix for the add_jan consequences.
    if (with(df, anyNA(c(confidence, code)))) {
        df %>% 
            filter(is.na(confidence) | is.na(code)) %>%
            getElement("historical_date") %>%
            substr(x = ., start = 6, stop = 10) %>% 
            {. == "01-01"} %>% 
            all() %>% 
            stopifnot(`dates that are missing are not only 01-01` = .)
    }

    # NA on critical columns
    stopifnot(!anyNA(df$coder_id))
    stopifnot(!anyNA(df$historical_date))
    # Duplicates
    stopifnot(`duplicates found on critical columns` =
        no_duplicates(df, c("country_text_id", "historical_date", "coder_id")))

    wide.df <- dcast(as.data.table(df),
            country_text_id + historical_date ~ coder_id,
            value.var = c("code", "confidence"), fill = NA) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        arrange(country_text_id, historical_date) %>%
        mutate(year = to_year(historical_date))

    return(wide.df)
}

interpolate_df <- function(df, interpolation_function = vutils::locf) {

    stopifnot(c("country_text_id", "year", "historical_date") %in% names(df))
    stopifnot(is.data.frame(df))
    stopifnot("Date" %in% class(df$historical_date), !is.na(df$historical_date))

	info("Interpolating within country-years.")
	# Create group index and row index
	df %<>% 
		arrange(country_text_id, historical_date) %>%
        # create group index for country
		mutate(id = seq_along(country_text_id)) %>%
		group_by(country_text_id, year) %>%
        # create year index within each country
		mutate(idx = cur_group_id()) %>% 
		ungroup()

	# Create matrix for faster computation
	m <- df %>% select(-country_text_id, -historical_date, -year) %>%
		as.matrix()

	# Interpolate each group column-wise using `locf`:
    # this fills NAs forward in time within each country-year
    # output is a list[idx] of matrices
	m_list <- lapply(unique(df$idx), function(i) {
		interpolation_function(m[m[, "idx"] == i, , drop = FALSE])
	})

	# Combine list of matrices into a data.frame
    do.call("rbind", m_list) %>%
		as.data.frame() %>%
		select(-idx) %>%
		# Merge in reference columns
		left_join(select(df, id, country_text_id, year, historical_date),
			by = "id") %>%
		select(country_text_id, historical_date, year, everything()) %>%
		select(-id) %>% 
        # Make confidence columns integer again
		mutate(across(contains("confidence"), as.integer)) %>%
        arrange(country_text_id, historical_date) %>% 
        return(.)
}

# only interpolate if the next year has at least one non-NA observation
locf_helper <- function(v) {
    # if all in the next year are NA return vector
    if (all(is.na(v[2:length(v)]))) {
        return(v)
    } else {
        return(locf(v))
    }
}

# apply locf_helper to each column of a matrix
locf_helper_matrix <- function(m) {
    stopifnot(is.matrix(m))
	apply(m, 2, locf_helper)		
}

interpolate_across_years <- function(df) { 
    
	info("Interpolating across country-years.")
	df_last_year <- 
		df %>%
		select(country_text_id, historical_date, year, everything()) %>%
		mutate(group_year = year + 1, last_id = TRUE) %>%
		group_by(country_text_id, year) %>%
		arrange(country_text_id, historical_date) %>%
		filter(row_number() == n())


	df_both <- 
		bind_rows(df, df_last_year) %>%
        # group_year is year+1 for all added rows from df_last_year, otherwise
        # it is year
		mutate(group_year = ifelse(is.na(group_year), year, group_year)) %>%
		arrange(country_text_id, historical_date) %>%
        # last_id is only TRUE for added rows
		mutate(last_id = ifelse(is.na(last_id), FALSE, last_id))
	
	df_both %>%
		select(-year) %>%
        # exchange year for group_year
		rename(year = group_year) %>%
        # if whole column is NA, return NA, otherwise locf
		interpolate_df(., interpolation_function = locf_helper_matrix) %>%
		filter(!last_id) %>%
		select(-last_id, -year) %>%
		mutate(year = to_year(historical_date)) %>%
		select(country_text_id, historical_date, year, everything()) %>% {
	        stopifnot(nrow(.) == nrow(df)); .
        } %>% 
        return(.)
}

interpolate_steps <- function(df) {
    df %>%
        # first interpolate within a year
        interpolate_df %>%
        # then interpolate across years, using the last observation from the 
        # previous year only
        interpolate_across_years %>%
        return(.)
}

interpolate_wide <- function(wide.df, qtable, TASK_NAME) {
    els <- qtable$els[qtable$name == TASK_NAME]  
    stopifnot(length(els) == 1)

    if (isTRUE(els)) {
        info("ELS variables are not interpolated: returning input.")
    } else {
        wide.df <- interpolate_steps(wide.df)
    }
    return(wide.df)
}

wide_to_matrices <- function(wide.df, qtable, TASK_NAME, vig = FALSE) {

    stopifnot(all(c("country_text_id", "historical_date") %in% names(wide.df)))

    if (vig) {
        country_dates <- paste0("A_", wide.df$country_text_id, " ", wide.df$historical_date)
    } else {
        country_dates <- paste(wide.df$country_text_id, wide.df$historical_date)
    }
    wdata <- select(wide.df, dplyr::matches("code")) %>% data.matrix
    conf_mat <- select(wide.df, dplyr::matches("confidence")) %>% data.matrix

    # Self-reported confidences are from 0 - 100; MM expects 0 - 1.
    conf_mat <- conf_mat / 100

    # MM data should be 1-indexed so add 1; however, we don't do this for
    # percentage variables since they will be bootstrapped. Postgres
    # inserts -1 for missingness though so replace with NA.
    if (isTRUE(qtable$question_type != "R") & !vig) {
        wdata <- wdata + 1
    } else {
        missing <- wdata == -1

        is.na(wdata) <- missing
        is.na(conf_mat) <- missing
    }

    colnames(wdata) <- sub("code_", "", colnames(wdata))
    rownames(wdata) <- country_dates

    colnames(conf_mat) <- sub("confidence_", "", colnames(conf_mat))
    rownames(conf_mat) <- country_dates

    return(list(wdata = wdata, conf_mat = conf_mat, country_dates = country_dates))
}

main <- function(df, qtable, coder_table, ctable, TASK_NAME, lateral_dates) {
    
    # Remove if a coder has only lateral scores for this question
    df <- remove_lateral_only_coders(df, coder_table)

    # Lateral scores
    lateral_df <- define_lateral(df, coder_table)
    if (nrow(lateral_df) > 0) {

		# Subset lateral jan-01 dates that we inserted in the add_jan_dates
        # We do not want them as lateral vignettes as it would inflate the number observations
		lateral_dates <- lateral_dates[
            with(lateral_dates, added_jan_dates),
            c("country_id", "historical_date")]

        # Regular and lateral scores on lateral dates transformed to matrices
		# Each row is now a country-date and columns are code and confidence
		# per coder_id, e.g. code_01, confidence_01 for coder number 01.
        lateral_vign <-
            # Identify the lateral vignettes from df
            semi_join(df, distinct(lateral_df, country_id, historical_date),
                by = c("country_id", "historical_date")) %>%
			# Remove added jan 01 dates from lateral vignettes
			anti_join(lateral_dates, by = c("country_id", "historical_date")) %>%
            to_wide_form(ctable) %>%
			# Wide to matrices returns a list of three matrices:
			# one for the code, one for confidences, and one 
			# vector of country-dates
            wide_to_matrices(., qtable, TASK_NAME, vig = TRUE)
    } else {
        lateral_vign <- NULL
    }
    
    interpolated_list <- 
		# Remove lateral scores (we have the separate lateral_vign object)
		# lateral_df is a subset of df and therefore we can merge by `id`.
        anti_join(df, lateral_df, by = "id") %>%
		# Each row is now a country-date and columns are code and confidence
		# per coder_id, e.g. code_01, confidence_01 for coder number 01.
        to_wide_form(ctable) %>%
		# Interpolate coder scores
        interpolate_wide(., qtable, TASK_NAME) %>%
		# Wide to matrices returns a list of three matrices:
		# one for the code, one for confidence, and one vector of country-dates
        wide_to_matrices(., qtable, TASK_NAME)
	
	# Append lateral vignettes
    interpolated_list[["lateral_as_vignettes"]] <- lateral_vign
    return(interpolated_list)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    qtable <- load_qtable() |>
        subset(name == TASK_NAME, select = c(name, els, question_type, class))
    ctable <- load_country() |> 
        subset(select = c(country_id, country_text_id, name))
	lateral_dates <-
        read_file(file.path(ROOT, "download", "lateral_dates.rds")) |> 
		subset(select=c(lateral_id, country_id, historical_date, added_jan_dates))

    objs <- find_dep_files(TASK_ID, db)
    coder_table <- objs[[sprintf("%s_coder_table",TASK_NAME)]][[TASK_NAME]]
    module_name <- names(objs)[!names(objs) %in% {sprintf("%s_coder_table",TASK_NAME)}]
    df <- objs[[module_name]][[TASK_NAME]]

    # Run
    collectedInputs <-
        named_list(df, qtable, coder_table, ctable, TASK_NAME, lateral_dates)
    do.call(main, collectedInputs) %>%
        list(.) %>%
        setNames(TASK_NAME) %>% 
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done interpolating " %^% TASK_NAME)
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/mm_prep/test_interpolate_coders.R") %>%
		check_test_file_output()
}

