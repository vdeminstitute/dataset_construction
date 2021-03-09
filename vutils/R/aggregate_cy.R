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

#' Apply a function per group and combine results
#'
#' Simple wrapper around the conventional split-lapply-unsplit pattern
#' that, unlike \code{\link{by}}, returns a combined object.
#'
#' @param x An R object, either \code{data.frame} or \code{matrix}
#' @param factors Factor or list of factors passed to
#'     \code{\link{split}} and \code{\link{unsplit}}
#' @param fn Function to apply to each group
#' @param mc.cores Number of cores to use as parallel jobs.
#' @param ... Additional arguments passed to
#'     \code{\link[parallel]{mclapply}}
#'
#' @section Warning: \code{by_split} assumes that the subsets returned
#'     by the function \code{f} are of the same class as the
#'     original object.
#'
#' @return Object of same class as \code{x}.
#'
#' @examples
#' df <- data.frame(x = c(1, 1, 2, 2), y = c(1, NA, NA, 4))
#' by_split(df, df$x, locf)
#'
#' @export
by_split <- function(x, factors, fn, mc.cores = 1, ...) {
    if (class(fn) != "function")
        stop("Invalid function")

    UseMethod("by_split")
}

#' @export
by_split.matrix <- function(x, factors, fn, mc.cores = 1, ...) {
    ll <- split.data.frame(x, factors) %>%
        parallel::mclapply(fn, mc.cores = mc.cores, ...)

    mc_assert(ll)
    names(ll) <- NULL

    do.call(rbind, ll)
}

#' @export
by_split.data.frame <- function(x, factors, fn, mc.cores = 1, ...) {
    ll <- split(x, factors) %>%
        parallel::mclapply(fn, mc.cores = mc.cores, ...)

    mc_assert(ll)
    names(ll) <- NULL
    len <- vapply(ll, nrow, numeric(1)) %>% sum

    if (len == nrow(x))
        unsplit(ll, factors)
    else
        do.call(rbind, ll)
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
        warn("There is already a year column... \n I am dropping it.")
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

    dates_v <- prep_arg(substitute(dates), p) %>% as.Date
    if (length(dates_v) != nrow(x))
        stop("Mismatch length between dates and x", call. = F)

    if (missing(by)) {
        aggregated.df <- by_year(x, dates_v, day_mean) %>%
            do.call(rbind, .)

        return(aggregated.df)
    }

    by_v <- prep_arg(substitute(by), p)
    if (length(by_v) != nrow(x))
        stop("Mismatched length between `by` and x", call. = F)

    dates.ll <- split(dates_v, by_v)
    ll <- split(x, by_v) %>%
        parallel::mcMap(function(sub.df, sub_dates) {
            by_year(sub.df, sub_dates, day_mean) %>%
                do.call(rbind, .)
            }, ., dates.ll, ...)

    mc_assert(ll)
    aggregated.df <- do.call(rbind, ll)

    bynames <- sub("[.]\\d*$", "", row.names(aggregated.df))
    aggregated.df[[deparse(substitute(by))]] <- bynames
    row.names(aggregated.df) <- NULL

    if ("country_id" %in% names(aggregated.df) & class(aggregated.df$country_id) == "numeric") {
        aggregated.df[["country_id"]] <- as.numeric(as.integer(aggregated.df[["country_id"]]))
    }

    aggregated.df
}
