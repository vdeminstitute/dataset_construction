#' Check for v-dem date columns and add/modify them where applicable.
#'
#' Checks for the given data.frame if the "year" column exists and if it is of type numeric.
#' If it does not exist it is created from the "historical_date" column and in any case set
#' to type numeric.
#' The function also checks for a "historical_date" column and creates it from the "year" column if
#' necessary and makes sure it is of type Date.
#'
#' @param df A v-dem data.frame in wide format.
#'
#' @return A data.frame similar to the input, but with date columns adjusted.
#'
#' @export
add_date_cols <- function(df) {
    if("year" %in% colnames(df)) {
        if(class(df$year) != "numeric") df$year <- as.numeric(df$year)
    } else df$year <- as.numeric(substr(df$historical_date, 1, 4))
    if("historical_date" %in% colnames(df)) {
        if(class(df$historical_date) != "Date")
            df$historical_date <- as.Date(df$historical_date)
    } else df$historical_date <- as.Date(paste0(df$year, "-12-31"))
    df
}

#' @export
add_country_cols <- function(df, country) {
    if (all(c("country_text_id", "country_id") %in% names(df)))
        return(as.data.frame(df, stringsAsFactors = F))
    stopifnot(any(c("country_text_id", "country_id") %in% names(df)))


    if("country_id" %in% names(df)) {
        return(dplyr::left_join(df, dplyr::select(country, country_id, country_text_id),
            by = c("country_id")) %>% as.data.frame(stringsAsFactors = F))
    }

    if ("country_text_id" %in% names(df)) {
        return(dplyr::left_join(df, dplyr::select(country, country_id, country_text_id),
            by = c("country_text_id")) %>% as.data.frame(stringsAsFactors = F))
    }
    stop("We should not hit this case")
}


#' Translate identifiers
#'
#' \code{trans} translates a vector of identifiers from one format to
#' another according to a given translation table (\emph{e.g.},
#' translating question IDs to question name).
#'
#' @param v Vector of any type.
#' @param to Character. Corresponding identifier format to translate
#'     our vector \code{v} to.
#' @param ttable Translation table. Must be a \code{data.frame}
#'     containing the columns specified by \code{to} and \code{by}
#' @param by Character. Column key for translation.
#'
#' @examples
#' \dontrun{
#' # Given a vector of question IDs saved to qids and we want the
#' # corresponding tag names using the translation table `qtable`.
#' trans(qids, to = "name", ttable = qtable, by = "question_id")
#' }
#'
#' @family translation helper functions
#' @seealso \code{\link{to_qids}} for question IDs,
#'     \code{\link{to_qnames}} for question names,
#'     \code{\link{to_cids}} for country IDs, \code{\link{to_cnames}}
#'     for country names.
#'
#' @export
trans <- function(v, to = NULL, ttable = NULL, by = NULL) {
    for (x in c("to", "ttable", "by"))
        if (is.null(eval(substitute(x))))
            stop("Missing argument: " %^% x)

    columns <- colnames(ttable)
    if (!by %in% columns | !to %in% columns)
        stop("Missing `to` or `by` column in translation table")

    if (any(!v %in% ttable[[by]]))
        stop("Missing values in ttable from " %^% deparse(substitute(v)))

    ttable[[to]][match(v, ttable[[by]])]
}

#' Translation helper functions
#'
#' Convenience functions to translate several common identifiers at V-Dem.
#'
#' @param v \code{CharacterVector} or \code{NumericVector}
#' @param ttable Translation table. See \link{trans}.
#'
#' @details The listed helper functions are fairly rigid in their
#'     assumptions, especially in the case of assumed column names in
#'     the translation table.
#'
#'     It's worth noting that the function \code{to_qlabels} expects
#'     not the \code{question} table as \code{ttable}, but rather the
#'     \code{codebook} table, which contains the canonical short-form
#'     question description texts. Further, it includes a call to
#'     \code{\link{get_root}} so that it can conveniently be called on
#'     the columns of the final dataset.
#'
#' @examples
#' \dontrun{
#' tags <- c("v2clacfree", "v2elpaidig")
#' qids <- to_qids(tags, qtable)
#' to_qnames(qids)
#' }
#'
#' @seealso \code{\link{trans}}
#' @name trans_helpers
NULL

#' @describeIn trans_helpers Translate country text IDs to country IDs
#' @export
to_cids <- function(v, ttable) {
    trans(v, to = "country_id", ttable = ttable, by = "country_text_id")
}

#' @describeIn trans_helpers Translate country text IDS to country names
#' @export
to_cnames <- function(v, ttable) {
    trans(v, to = "name", ttable = ttable, by = "country_text_id")
}

#' @describeIn trans_helpers Translate country IDs to country text IDs
#' @export
to_ctext_ids <- function(v, ttable) {
    trans(v, to = "country_text_id", ttable = ttable, by = "country_id")
}

#' @describeIn trans_helpers Translate question tag names to question IDs
#' @export
to_qids <- function(v, ttable) {
    trans(v, to = "question_id", ttable = ttable, by = "name")
}

#' @describeIn trans_helpers Translate question IDs to question tag names
#' @export
to_qnames <- function(v, ttable) {
    trans(v, to = "name", ttable = ttable, by = "question_id")
}


