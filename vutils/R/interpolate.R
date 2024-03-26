#' Front-fill an object
#'
#' S3 generic function that front-fills by carrying the last
#' observation forward. For DataFrames and Matrices, this is done
#' column-wise.
#'
#' @param x Object to front fill.
#'
#' @examples
#' v <- c(NA, 1, NA, 2, 2)
#' locf(v)
#'
#' df <- data.frame(x = c(1, NA, NA))
#' (out <- locf(df))
#'
#' # The original object should be preserved
#' identical(df, out)
#'
#' m <- matrix(1:6, 3, 2)
#' m[1, 2] <- NA
#'
#' # This should still be identical to m
#' locf(m)
#'
#' @family fill functions
#' @export
locf <- function(x) UseMethod("locf")

#' Interpolation
#'
#' S3 generic function that back fills an object if the last element
#' is the only non-missing observation. Otherwise, front filling by
#' carrying the last observation forward is done, similar to
#' \code{\link{locf}}. For DataFrames and Matrices, this is all done
#' column-wise.
#'
#' @param x Object to interpolate.
#'
#' @details The differences between \code{locf} and \code{interpolate}
#'     are frustratingly trivial; however, we need the
#'     \code{interpolate} functions in situations where the only
#'     observation is at the default date ("-12-31"), which should
#'     then represent the entire year.
#'
#'     For example, when conforming multiple country-date level
#'     variables, missingness introduced by merging should be first
#'     interpolated before being expanded so as to better reflect the
#'     observation at the default date representing the entire year,
#'     rather than the previous year's default date observation being
#'     carried forward until "-12-30".
#'
#' @examples
#' df <- data.frame(x = c(1, NA, 2),
#'                  y = as.Date(c("1900-12-31", "1901-10-12", "1902-12-31")))
#'
#' transform(df, x = interpolate(x))
#'
#' # For comparison purposes
#' transform(df, x = locf(x))
#'
#' @family fill functions
#' @export
interpolate <- function(x) UseMethod("interpolate")

#' Gap Index
#'
#' \code{create_idx} returns a grouping index as a numeric vector
#' denoting sequential dates, \emph{i.e.} groups separated by year
#' gaps. The resulting vector can be coerced to a factor for use with
#' \code{split} or \code{dplyr::group_by}.
#'
#' @param x Vector of years or Date objects.
#'
#' @details Where this is most useful is when we fill in missing
#'     observations and we want to make sure we never fill across year
#'     gaps. \code{create_idx} returns a grouping vector that can be
#'     used when splitting before filling.
#'
#'     The actual values of the returned vector are inconsequential
#'     and are only useful for identifying groups.
#'
#'     As an example, V-Dem does not code the occupation of Germany
#'     after WWII. To ensure that values prior to 1946 are not used to
#'     fill in missingness after 1949, the data should be split by
#'     gaps by creating an index column denoting the two groups.
#'
#' @examples
#' dates <- as.Date(c("1900-01-01", "1901-12-31", "1903-01-01"))
#' create_idx(dates)
#'
#' years <- c(1900, 1901, 1905, 1907)
#' create_idx(years)
#'
#' @export
create_idx <- function(x) UseMethod("create_idx")

#' @export
create_idx.default <- function(x) {
    if (any(is.na(x)))
        stop("Invalid input vector contained NA", call. = F)

    if (!is.numeric(x) | any(floor(log10(x)) + 1 != 4))
        stop("Invalid years", call. = F)

    cumsum(c(T, diff(x) > 1))
}

#' @export
create_idx.Date <- function(x) {
    if (any(is.na(x)))
        stop("Invalid input vector contained NA", call. = F)

    cumsum(c(T, diff(x) > 366))
}


#' @export
add_gap_idx <- function(df, utable) {
    stopifnot(c("year", "country_id") %in% names(df))
    stopifnot(!"gap_idx" %in% names(df))
    outdf <- 
		dplyr::left_join(df, 
						 dplyr::select(utable, country_id, year, gap_idx),
 	                    by = c("country_id", "year")) %>%
		# Fill gap_idx for pre-historical period!
		dplyr::group_by(country_id) %>%
		dplyr::arrange(desc(historical_date)) %>%
		dplyr::mutate(gap_idx = locf(gap_idx)) %>%
		dplyr::ungroup(.) %>%
		dplyr::arrange(country_id, historical_date)
	stopifnot(`gap_idx has missing values!` = !is.na(outdf$gap_idx))
	return(outdf)
}



#' @export
interpolate_components <- function(df, cols, utable, keep_nan = TRUE, 
	coder_level = FALSE) {
	
	YEAR_CREATED <- FALSE
	stopifnot(is.data.frame(df))
    stopifnot(c("historical_date", "country_id") 
		%in% names(df))

	is.nan.data.frame <- function(df) {
		sapply(df, is.nan) %>% cbind(.)
	}

	if (keep_nan) {
		bool_m <- is.nan.data.frame(df)
		df[bool_m] <- Inf
	}

	# If year column is missing create it (for utable merge)
	if (is.null(df$year)) {
		df$year <- to_year(df$historical_date)
		YEAR_CREATED <- TRUE
	}

	# If year column has missing fill it.
	if (anyNA(df$year)) {
		df$year[is.na(df$year)] <- to_year(df$historical_date[is.na(df$year)])
	}


	if (coder_level) {
		group_cols <- c("country_id", "gap_idx", "coder_id")
	} else {
		group_cols <- c("country_id", "gap_idx")
	}

	df %<>%
		add_gap_idx(utable) %>%
		dplyr::group_by(dplyr::across(group_cols)) %>%
		dplyr::arrange(country_id, historical_date) %>%
		dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(cols), .fns = locf))
	
	stopifnot(`The coding period is outside utable!` = !is.na(df$gap_idx))

	if (keep_nan) {
		lapply(cols, function(v, df_) {
			df[[v]][is.infinite(df_[[v]])] <<- NaN
		}, df_ = df) %>% invisible
	}

	df %<>% dplyr::ungroup(.) %>% dplyr::select(-gap_idx) %>% as.data.frame

	if (YEAR_CREATED) {
		df %<>% dplyr::select(-year)
	}


	return(df)
}
