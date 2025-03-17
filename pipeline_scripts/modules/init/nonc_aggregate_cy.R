#!/usr/bin/env Rscript

# ==========================================================================
# Create country_year version, by default aggregate obs by `last`,
# except for event-specific/ELS (max) and ratio variables
# (day-weighted mean).
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
add_country_date_columns <- function(df, country) {

    df <-
        vutils::add_country_cols(df, country) |> 
        vutils::add_date_cols()
    df[["country_text_id"]] <- NULL

    return(df)
}

# Extract the cy_aggregation method from the qtable
find_aggregation_method <- function(qtable, TASK_NAME) {

    lastVars <- c("v2exparhos", "v2expothog", "v2exnamhos", "v2exnamhog", 
        "v2exhoshog", "v3exnamhos", "v3exnamhog")

    hardcodedMaxVars <- c("v3eldirelc", "v3eldireuc", "v3eldirepr",
            "v2ellostts", "v2ellovttm", "v3ellostsl", "v3ellostss", 
            "v2x_hosabort", "v2x_legabort")
    stopifnot(grepl("^v[23]", TASK_NAME))

    # Extract variables with maximum aggregation
    maxVars <- qtable$name[with(qtable,
        (!is.na(date_specific) & !question_type %in% c("R", "T") & !name %in% lastVars) |
        name %in% hardcodedMaxVars |
        grepl("elecreg", name))]
    
    if (length(maxVars) > 0) {
        stopifnot(isTRUE(qtable[["cy_aggregation"]] == "Maximum"))
        info(sprintf("Aggregating %s to country-year by maximum value", TASK_NAME))
        return("max")
    }
    
    # Ratio variables / percentages
    ratioVars <- qtable$name[with(qtable, question_type == "R" &
        index_type == "interval" & is.na(date_specific))]

    if (length(ratioVars) > 0) {
        stopifnot(isTRUE(qtable[["cy_aggregation"]] == "Day-weighted mean"))
        info(sprintf("Aggregating %s to country-year by ratio", TASK_NAME))
        return("ratio")
    }

    # Residual category
	stopifnot(isTRUE(qtable[["cy_aggregation"]] == "Last"))
    info(sprintf("Aggregating %s to country-year by last value", TASK_NAME))
    return("last")
}

# Ratio method
aggr_ratio <- function(df) {
    out <- cy.day_mean(df, dates=historical_date, by=country_id, mc.cores = 1)
    out[["country_id"]] <- as.numeric(out[["country_id"]])
    return(out)
}

# Maximum method
aggr_max <- function(df) {
    
    df[["historical_date"]] <- NULL
    v <- names(df)[!names(df) %in% c("country_id", "year")]
    out <- 
        split(df, list(df[["country_id"]], df[["year"]]), drop = TRUE) |> 
        lapply(X=_, function(x, v) {
            out <- x[1, c("country_id", "year")]
            out[[v]] <- collect_max(x[[v]])
            stopifnot(nrow(out) == 1L)
            return(out)
            },v=v) |> 
        do.call(what=rbind.data.frame, args=_) |>
        vbase::organiseRows(df=_, country_id, year)
    row.names(out) <- seq(1, nrow(out))

    return(out)
}

# Last method
aggr_last <- function(df) {

    df[["historical_date"]] <- NULL
    v <- names(df)[!names(df) %in% c("country_id", "year")]
    out <- 
        split(df, list(df[["country_id"]], df[["year"]]), drop = TRUE) |> 
        lapply(X=_, function(x, v) {
            out <- x[1, c("country_id", "year")]
            out[[v]] <- collect_last(x[[v]])
            stopifnot(nrow(out) == 1L)
            return(out)
            },v=v) |> 
        do.call(what=rbind.data.frame, args=_) |>
        vbase::organiseRows(df=_, country_id, year)
    row.names(out) <- seq(1, nrow(out))

    return(out)
}

# For APS we drop january first
dropJanuaryFirst <- function(df, qtable, TASK_NAME, dfJan) {
    
	stopifnot(`Despite expectation, cannot find added rows.` = 
		!is.null(dfJan))
    stopifnot(`dfJan is not a data.frame.` = is.data.frame(dfJan))
    stopifnot(`Not all dates in jan_df are yyyy-01-01.` = 
        substr(dfJan[["historical_date"]], 6, 10) == "01-01")
	stopifnot(`Only for appointment date specific variables` = 
		isTRUE(qtable[["aps"]]))
	stopifnot(isTRUE(qtable[["cy_aggregation"]] == "Maximum"))

    dfId <- interaction(df[, c("country_id", "historical_date")], drop = TRUE)
    janId <- interaction(dfJan[, c("country_id", "historical_date")], drop = TRUE)
    stopifnot(!anyNA(dfId), !anyNA(janId))
    out <- df[!dfId %in% janId, ]

    stopifnot(`output after anti join has zero rows` = nrow(out) > 0)
    return(out)
}

main <- function(df, TASK_NAME, qtable, country, dfJan) {
    
    df <-
        add_country_date_columns(df, country) |> 
        vbase::organiseRows(df=_, country_id, historical_date)

    aggregation_method <- find_aggregation_method(qtable, TASK_NAME)

    # Do not aggregate using max before removing added january firsts
    if (aggregation_method == "max") {
		if (isTRUE(qtable[["aps"]])) {
			df <- dropJanuaryFirst(df, qtable, TASK_NAME, dfJan) 
		}
        dfCy <- aggr_max(df)
    }
    # Other methods are not affected by added january firsts
    if (aggregation_method == "ratio") {
        dfCy <- aggr_ratio(df)
    }     
    if (aggregation_method == "last") {
        dfCy <- aggr_last(df)
    }

    with(dfCy, stopifnot(!is.na(country_id), !is.na(year)))    
    return(list(cd = df, cy = dfCy))
}

# Functions to extract dependencies
getJanuaryFirst <- function(objs, qtable, utable, TASK_NAME) {
	
    stopifnot(isTRUE(qtable[["aps"]]))

    # v2-variables that are merged with their historical counterpart,
    # need their January first dependencies from both contemporary and historical

    if(isTRUE(qtable[["hist_merged"]]) && grepl("^v2", qtable[["name"]])) {
        info(sprintf("Using both v2 and v3 rows for January first"))
        
        v2Jan <-
            objs[[grep("^v2.*add_jan_dates$", value = TRUE, x = names(objs))
				]][["added_rows"]][, c("country_id", "historical_date")]
        
        v3Jan <-
            objs[[grep("^v3.*add_jan_dates$", value = TRUE, x = names(objs))
				]][["added_rows"]][, c("country_id", "historical_date")]
                
        dfJan <- conthistmergeJanuaryFirst(v2Jan, v3Jan, utable)

    # All other APS variables only require its own added january first observations
    } else {
        dfJan <-
            objs[[paste0(TASK_NAME, "_add_jan_dates")]][["added_rows"]][, c("country_id", "historical_date")]
    }

	dfJan <- vbase::organiseRows(dfJan, country_id, historical_date)

	return(dfJan)
}

conthistmergeJanuaryFirst <- function(v2Jan, v3Jan, utable) {
	
	# (1) The v3 dependency needs to be cleaned by utable: we drop the overlap observations.
    # -- We drop them because we prioritise the contemporary project.
    # (2) Keep observations where project is NA as they represent pre-coding observations.
    stopifnot(!is.null(v2Jan), !is.null(v3Jan))

    v3Jan[["year"]] <- to_year(v3Jan[["historical_date"]])
    v3Jan <- merge(
        x = v3Jan, y = utable,
        by = c("country_id", "year"), all.x = TRUE)
    v3Jan <- v3Jan[!v3Jan[["project"]] %in% c("overlap", "contemporary-only"), ]
    
    stopifnot(`filtering by utable has gone wrong` =
        !v3Jan[["project"]] == "overlap" | is.na(v3Jan[["project"]]))
    
    v3Jan <- v3Jan[, c("country_id", "historical_date")]

    # Bind v2Jan and v3Jan to define the january first object
    dfJan <- rbind.data.frame(v2Jan, v3Jan)

    stopifnot(`Duplicated in df_jan from binding rows` = !duplicated(dfJan))
	return(dfJan)
}

# Run script
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    qtable <- load_qtable()
    qtable <- qtable[qtable[["name"]] == TASK_NAME, ]
    country <- load_country()
    utable <- load_country_unit()[, c("country_id", "year", "project")]
    objs <- find_dep_files(TASK_ID, db)
    df <- objs[[names(objs)[!grepl("add_jan_dates", names(objs))]]][[TASK_NAME]]
	# For appointment-specific variables aggregated by max, we need to remove the
    # previously added January first dates that were inserted in the add_jan_dates module.
	if (isTRUE(qtable[["aps"]])) {
		dfJan <- getJanuaryFirst(objs, qtable, utable, TASK_NAME)
	} else {
		dfJan <- NULL
	}
    stopifnot(nrow(qtable) == 1L)

    # Run
    collectedInputs <- named_list(df, TASK_NAME, qtable, country, dfJan)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
    info("Done with nonc_aggregate_cy.")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_nonc_aggregate_cy.R") %>%
		check_test_file_output()
}
