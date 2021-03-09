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
interpolate_vdem_style <- function(df, col, utable) {
    stopifnot(is.data.frame(df))
    stopifnot(c("historical_date", "year", "country_id") %in% names(df))
    stopifnot(!"fill_col" %in% names(df))

    out <- df %>%
        add_gap_idx(utable) %>%
        dplyr::group_by(country_id) %>%
        # interpolate gap_idx for historical years not in utable!
        # This should not interpolate into gaps, that is dangerous.
        dplyr::arrange(dplyr::desc(historical_date)) %>%
        dplyr::mutate(gap_idx = locf(gap_idx)) %>%
        dplyr::ungroup() %>%
        #{stopifnot(!anyNA(.$gap_idx))} %>%
        dplyr::group_by(country_id, year) %>%
        dplyr::arrange(country_id, historical_date) %>%
        dplyr::mutate(fill_col = interpolate(!!dplyr::sym(col))) %>%
        dplyr::group_by(country_id, gap_idx) %>%
        dplyr::arrange(country_id, historical_date) %>%
        dplyr::mutate(fill_col = locf(fill_col)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-gap_idx) %>%
        as.data.frame(stringsAsFactors = F)
}



#' @export
interpolate_vdem_col <- function(df, col, utable) {
    stopifnot(is.data.frame(df))
    stopifnot(c("historical_date", "year", "country_id") %in% names(df))
    out <- interpolate_vdem_style(df, col, utable)
    out[[col]] <- out$fill_col
    out$fill_col <- NULL
    out
}

#' @export
fill_special_cols <- function(df, utable) {
    colu <- c("v2lgbicam", "v2exhoshog", "v2expathhs", "v2expathhg",
      "v2lgello", "v2lginello", "v2lgelecup", "v2lginelup",
      "v2exapup", "v2exapupap")
    lapply(colu, function(v) {
        if (v %in% names(df))
            df <<- interpolate_vdem_col(df, v, utable)
    }) %>% invisible

    df
}


#' @export
add_gap_idx <- function(df, utable) {
    stopifnot(c("year", "country_id") %in% names(df))
    stopifnot(!"gap_idx" %in% names(df))
    dplyr::left_join(df, dplyr::select(utable, country_id, year, gap_idx),
                     by = c("country_id", "year"))
}


