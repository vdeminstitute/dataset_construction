#' Country-year aggregation helper functions
#'
#' Collection of helper functions meant to used in conjuction with
#' \code{dplyr::summarise_all} when aggregating column-wise from the
#' country-date to country-year level.
#'
#' @param v Vector of any type.
#'
#' @details When aggregating per year, the base R functions may return
#'     a different type than the original input vector. This will
#'     cause the \code{summarise} family of functions to error
#'     out. The collection of helper functions simply take care of
#'     missingness and \code{NA} as appropriate.
#'
#' @name collect_aggregation
NULL

#' @describeIn collect_aggregation Aggregate by max
#' @export
collect_max <- function(v) {
    if (length(v) == 1)
        return(v)

    # Grab the first element if missing instead of NA, so we don't
    # return NA_real_
    if (all(is.na(v))) v[1] else max(v, na.rm = T)
}

#' @describeIn collect_aggregation Aggregate by last
#' @export
collect_last <- function(v) {
    if (length(v) == 1)
        return(v)

    v[v == ""] <- NA

    if (any(!is.na(v)))
        utils::tail(stats::na.omit(v), n = 1)
    else
        v[1]
}

#' @describeIn collect_aggregation Aggregate by first
#' @export
collect_first <- function(v) {
    if (length(v) == 1)
        return(v)

    v[v == ""] <- NA

    if (any(!is.na(v)))
        utils::head(stats::na.omit(v), n = 1)
    else
        v[1]
}


#' @describeIn collect_aggregation Aggregate by mean
#' @export
collect_mean <- function(v) {
    if (inherits(v, "character"))
        stop("cannot take mean of character vector", call. = F)

    if (length(v) == 1)
        return(v)

    if (all(is.na(v))) v[1] else mean(v, na.rm = T)
}

#' Day weighted mean
#'
#' Computes the day weighted arithmetic mean of either a
#' \code{data.frame} or \code{matrix}. The weights are taken from the
#' day difference of the given \code{DateVector} and the mean is
#' calculated column-wise.
#'
#' @param x A \code{data.frame} of numeric/integer columns or a
#'     numeric/integer \code{matrix} object.
#' @param dates Date vector
#' @param na_rm Whether or not to remove NA values when calculating
#'     the mean
#'
#' @examples
#' df <- data.frame(x = c(1, 2, 3), y = c(1L, 2L, 3L))
#' dates <- as.Date(c("1900-01-01", "1900-03-23", "1900-12-31"))
#'
#' day_mean(df, dates)
#'
#' @export
day_mean <- function(x, dates, na_rm = T) UseMethod("day_mean" )

#' @export
day_mean.numeric <- function(x, dates = NULL) {
	day_mean(as.matrix(x), dates = dates)
}

#' @export
day_mean.integer <- function(x, dates = NULL) {
	day_mean(as.matrix(x), dates = dates)
}


#' Year-wise day-weighted aggregation
#'
#' Split-apply-combine style year-wise aggregation from country-date
#' to country-year using the aggregate function \code{\link{day_mean}}.
#'
#' @param x Either a \code{data.frame} or \code{matrix}
#' @param dates \code{DateVector} evaluated within the context of
#'     \code{x} if \code{x} is a \code{data.frame} containing
#'     \code{date.col} as a column. Can also specify "row.names" to
#'     access dates stored as rownames attributes of \code{x}.
#' @param by Additional grouping variables to split \code{x} before
#'     applying the aggregation. Like \code{dates}, if \code{x} is a
#'     \code{data.frame}, then \code{by} will be evaluated within the
#'     context of \code{x}.
#' @param ... Additional arguments passed to
#'     \code{\link[parallel]{mcMap}}.
#'
#' @details \code{to_cy} aggregates from country-date level to
#'     country-year level by splitting the data year-wise based on the
#'     specified \code{DateVector}. The aggregation is done by the
#'     \code{day_mean} function, which takes a day-weighted average.
#'
#' @examples
#' df <- data.frame(historical_date = as.Date(c("1900-01-01", "1900-03-01", "1900-07-11")),
#'                  code = c(1, 2, 1))
#' cy.day_mean(df, historical_date)
#'
#' @export
cy.day_mean <- function(x, dates = NULL, by = NULL, ...) UseMethod("cy.day_mean")

#' @export
cy.day_mean.data.frame <- function(x, dates = NULL, by = NULL, ...) {
    if (!is.data.frame(x))
        x <- as.data.frame(x)

    if (nrow(x) < 1 | ncol(x) == 0) {
        warning("Nothing to aggregate")
        return(x)
    }

    if ("year" %in% colnames(x)) {
        info("There is already a year column that will be dropped.")
        x$year <- NULL
    }

    prep_arg <- function(arg, e) {
        name <- deparse(arg)
        v <- eval(arg, x, e)

        if (is.null(v))
            return(NULL)

        if (length(v) == 1 && is.character(v) && v == "row.names")
            v <- row.names(x)
        else if (name %in% colnames(x)) {
            x <<- x[, colnames(x) != name, drop = F]
        }

        v
    }

    if (missing(dates))
        stop("Missing dates argument", call. = F)

    # Save the parent.frame to pass as the enclos env for `prep_arg`
    # so we can find objects not residing within `x`
    p <- parent.frame()

    # by dates
    dates_v <- as.Date(prep_arg(substitute(dates), p))
    if (length(dates_v) != nrow(x))
        stop("Mismatch length between dates and x", call. = F)

    if (missing(by)) {
        aggregated.df <- do.call(rbind, by_year(x, dates_v, day_mean))
        return(aggregated.df)
    }

    # by units
    by_v <- prep_arg(substitute(by), p)
    if (length(by_v) != nrow(x))
        stop("Mismatched length between `by` and x", call. = F)

    ll <-
        parallel::mcMap(function(sub.df, sub_dates) {
            do.call(rbind, by_year(sub.df, sub_dates, day_mean))
            }, split(x, by_v), split(dates_v, by_v), ...)
    mc_assert(ll)
    
    aggregated.df <- do.call(rbind, ll)

    aggregated.df[[deparse(substitute(by))]] <- sub("[.]\\d*$", "", row.names(aggregated.df))
    row.names(aggregated.df) <- NULL

	# These next lines seems useless, but otherwise there is a bug in numeric values
	# being slightly different from integer values (e.g. 29.000001 vs. 29) and would not match.
	# This is a problem passed from C++ using Rcpp.
    if ("country_id" %in% names(aggregated.df) & class(aggregated.df$country_id) == "numeric") {
        aggregated.df[["country_id"]] <- as.numeric(as.integer(aggregated.df[["country_id"]]))
    }

    aggregated.df
}



#' @export
election_date_mm_stretch_country_date <- function(
	df, tag, utable, elecreg_cy, country) {

    # Prepare elecreg_cy
    elecreg_cy <- merge(
        elecreg_cy, 
        country[, c("country_id", "country_text_id")],
        by = "country_id", all.x = TRUE)
    elecreg_cy[["country_id"]] <- NULL
	
    # Prepare utable
    utable <- utable[, c("country_text_id", "year", "gap_idx", "project")]

    # Prepare df
    df[["year"]] <- vutils::to_year(df[["historical_date"]])
    outdf <- merge(
        df, 
        utable, 
        by = c("country_text_id", "year"), 
        all = TRUE)
    outdf <- merge(
        outdf, 
        elecreg_cy, 
        by = c("country_text_id", "year"), 
        all = TRUE)
    outdf <- vbase::organiseRows(outdf, country_text_id, year)
	# Set missing historical_date to 1st of year
	outdf$historical_date[is.na(outdf$historical_date)] <- 
		as.Date(paste0(outdf$year[is.na(outdf$historical_date)], "-01-01"), 
				format = "%Y-%m-%d")

    outdf <- vbase::organiseRows(outdf, country_text_id, historical_date)

    # Drop period 'contemporary-only' for v3-variables
    if (isTRUE(startsWith(tag, prefix = "v3"))) {
        info("Dropping contemporary-only")
        outdf <- outdf[!outdf$project %in% "contemporary-only", ]
    }

    # Drop project column
    outdf[["project"]] <- NULL

	# Frontfill gap_idx
	outdf %<>% 
		dplyr::group_by(country_text_id) %>%
		dplyr::arrange(desc(year)) %>%
		dplyr::mutate(gap_idx = locf(gap_idx)) %>%
		dplyr::ungroup(.) %>% 
        dplyr::arrange(country_text_id, historical_date)

	outdf %<>% 
        # Set v2x_elecreg to 0 when missing
		dplyr::mutate(v2x_elecreg = ifelse(is.na(v2x_elecreg), 0, v2x_elecreg)) %>% 
        dplyr::group_by(country_text_id) %>% 
        # Create index on elecreg that identifies coherent electoral regimes
        dplyr::mutate(elecreg_cumidx = vbase::create_index(v2x_elecreg)) %>% 
        dplyr::ungroup(.) %>%
        dplyr::arrange(country_text_id, historical_date)

	stopifnot(!anyNA(outdf$gap_idx))
	stopifnot(!anyNA(outdf$elecreg_cumidx))
	stopifnot(!anyNA(outdf$historical_date))
	stopifnot(!anyNA(outdf$year))
	stopifnot(!anyNA(outdf$v2x_elecreg))

    # Group on cumulative elecreg index and interpolate forwards
    # This will make CY longer than CD
	outdf %<>%
		dplyr::group_by(country_text_id, gap_idx, elecreg_cumidx) %>%
		dplyr::arrange(country_text_id, historical_date) %>%
		dplyr::reframe(dplyr::across(everything(), locf))
    # name 'median' is produced by the model.R script  
	outdf <- outdf[!is.na(outdf[["median"]]), , drop = FALSE]
	outdf %<>% dplyr::select(-year, -gap_idx, -v2x_elecreg, -elecreg_cumidx)
    stopifnot(!dplyr::is.grouped_df(outdf))

	return(as.data.frame(outdf))
}
