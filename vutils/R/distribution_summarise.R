#' Row/Column-wise Median and Standard Deviation
#'
#' Calculate the median and standard deviation by column or row given
#' a numeric matrix. \code{NA}s will not be removed.
#'
#' @param x A numeric matrix
#'
#' @return A numeric vector
#'
#' @examples
#' m <- matrix(1:9, 3, 3)
#' m[1, 1] <- NA
#'
#' colMedians(m)
#' rowSDs(m)
#'
#' @export
colMedians <- function(x) UseMethod("colMedians")

#' @rdname colMedians
#' @export
colSDs <- function(x) UseMethod("colSDs")

#' @rdname colMedians
#' @export
rowMedians <- function(x) UseMethod("rowMedians")

#' @rdname colMedians
#' @export
rowSDs <- function(x) UseMethod("rowSDs")

#' @export
hpd <- function(m, prob) {
    out <- coda::HPDinterval(coda::as.mcmc(m), prob = prob)

    suffix <- sub("^.*[.]", "", prob)
    colnames(out) <- c("codelow", "codehigh") %^% suffix

    out
}

#' Summarise posterior distributions
#'
#' \code{dist_summary} summarises a country-date matrix where each column
#' is a posterior distribution.
#'
#' @param m Matrix, either a coerced parameter from a \code{stanfit} object
#'     or output from the \code{osp} or \code{ord} transformation
#'     functions. Columns correspond to country-dates.
#' @param expanded.names Row names to expand the summarised time
#'     series to using \code{\link{stretch}}.
#' @param drop.vignettes Boolean. Whether to drop vignette rows prior
#'     to summarisation. Note, if set to \code{FALSE}, but
#'     \code{expanded.names} is not \code{NULL}, the function will
#'     return an error since vignette identifiers are not valid
#'     country-dates.
#' @param ... Additional arguments passed to the \code{\link{stretch}}
#'     function.
#'
#' @details \code{dist_summary} will preserve entire columns of
#'     \code{NA} under the assumption that they're missing
#'     country-dates; however, it will fail when only a single value
#'     within a column is \code{NA} since we never want partial
#'     missingness within our posterior distribution.
#'
#'     More importantly, why did we call this function
#'     \code{dist_summary} when there's clearly a much better option?
#'     Great question, besides to add to the general confusion
#'     surrounding this package, it's also nice to minimize conflicts
#'     with \code{dplyr}.
#'
#' @return DataFrame containing the mean, median, sd,
#'     codehigh/low68, codehigh/low95.
#'
#' @export
dist_summary <- function(m, expanded.names = NULL, drop.vignettes = T,
    utable = NULL, ...)  UseMethod("dist_summary")

#' @export
dist_summary.matrix <- function(m, expanded.names = NULL, drop.vignettes = T,
    utable = NULL, party = FALSE, ...) {
    if (isTRUE(drop.vignettes) & !is.null(colnames(m)))
        m <- m[, !is_vignette(colnames(m))]

    # Split missing and nonmissing columns since `hpd` will choke on NA
    missing_b <- colSums(is.na(m)) == nrow(m)
    nonmissing <- m[, !missing_b, drop = F]

    m_mean <- colMeans(nonmissing)
    m_median <- colMedians(nonmissing)
    m_sd <- colSDs(nonmissing)

    hpd95 <- hpd(nonmissing, .95)
    hpd68 <- hpd(nonmissing, .68)

    nonmissing_summary <- cbind(m_mean, m_median, m_sd, hpd68, hpd95)

    summarised_vars <- c("mean", "median", "sd", "codelow68", "codehigh68",
                        "codelow95", "codehigh95")

    # TODO: If there's no missing, we should avoid copying
    out <- matrix(NA, ncol(m), 7, dimnames = list(colnames(m), summarised_vars))
    out[!missing_b, ] <- nonmissing_summary

    if (!is.null(colnames(m))) {
        df <- if (!is.null(expanded.names))
                 stretch(out, expanded.names, utable = utable, party = party, ...) %>% as.data.frame
             else
                 as.data.frame(out)
        if (party) {
            df$country_text_id <- row.names(df) %>% sub(" \\d{4}-\\d{2}-\\d{2}$", "", x = .)
        } else {
            df$country_text_id <- row.names(df) %>% get_text_id(party)
        }
        
        df$historical_date <- row.names(df) %>% get_date(party)
        row.names(df) <- NULL

        return(df)
    } else if (!is.null(expanded.names))
        stop("Missing country-dates from original matrix", call. = F)

    as.data.frame(out)
}

#' @export
b_summary <- function(m) {

    m_mean <- colMeans(m)
    m_median <- colMedians(m)
    m_sd <- colSDs(m)
    hpd95 <- hpd(m, .95)
    hpd68 <- hpd(m, .68)

    out <- cbind(m_mean, m_median, m_sd, hpd68, hpd95, colnames(m))
    colnames(out) <- c("mean", "median", "sd", "codelow68", "codehigh68",
                       "codelow95", "codehigh95", "coder_id")
    rownames(out) <- NULL
    out
}

#' @export
fix_stat_columns <- function(df, varname) {
    df %<>% dplyr::select(-mean)
    colnames(df) <- dplyr::case_when(
        colnames(df) == "country_text_id" ~ "country_text_id",
        colnames(df) == "party_id" ~ "party_id",
        colnames(df) == "historical_date" ~ "historical_date",
        colnames(df) == "year" ~ "year",
        colnames(df) == "median" ~ varname,
        T ~ varname %^% "_" %^% colnames(df))
    df
}

#' @export
hli_summary <- function(full.ma, VARNAME) {
	info("Summarising posteriors for " %^% VARNAME)
	cd <- dist_summary(t(full.ma))
	cy <- cy.day_mean(cd, historical_date, country_text_id)
	cd <- fix_stat_columns(cd, VARNAME)
	cy <- fix_stat_columns(cy, VARNAME)
	return(list(cd = cd, cy = cy, thin_post = full.ma))
}