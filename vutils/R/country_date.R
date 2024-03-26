#' Country-date
#'
#' Checks whether a given string is properly formatted as a
#' country-date string.
#'
#' @param x Character vector
#'
#' @details A country-date string consists of a 1-3 lettered V-Dem
#'     country identifier, \code{country_text_id}, concatenated with a
#'     space to a year-month-day date, \code{historical_date}. The
#'     resulting string is mostly used as the rownames of matrices in
#'     order to identify each country-date observation.
#'
#' @return Logical vector.
#'
#' @examples
#' is_country_date(c("AFG 1900-01-01", "pilot_1_v1", "BFD 2001"))
#'
#' @family country-date functions
#' @export
is_country_date <- function(x, party = FALSE) {
    if (party) {
        grepl("^[A-Z]{3} \\d{6} \\d{4}-\\d{2}-\\d{2}$", x, perl = T)
    } else {
        grepl("^[A-Z]{3} \\d{4}-\\d{2}-\\d{2}$", x, perl = T)
    }
}

assert_str <- function(x, party = FALSE) {
    if (any(!is_country_date(x, party)))
        stop("Invalid country-date string", call. = F)
}


#' Vignette ratings
#'
#' Indicates whether a country-date represents a vignette.
#'
#' @param x Character vector of country-dates
#' @param na.rm Whether to set \code{NA} values to false.
#'
#' @details Vignettes are identified by unique constructed from the
#'     vignette type, threshold, and version.
#'
#' @return Logical vector
#'
#' @examples
#' is_vignette(c("AFG 1900-01-01", "historical_1_v2", "pilot_1_v1"))
#'
#' @family country-date functions
#'
#' @export
is_vignette <- function(x, na.rm = T) {
    b <- grepl("^(A|B)_", x)

    if (!isTRUE(na.rm))
        b[is.na(x)] <- NA

    b
}

#' Extract party_id from the string
#'
#' @param x Character vector
#'
#' @return Integer vector of length 1 to 6 separated by space. 
#' 
#' @examples
#' get_cpid("RUS 2244 2016-09-18")
#' @family country_date functions
#' 
#' @export
get_party_id <- function(x) {
    assert_str(x, party = TRUE)
    as.numeric(gsub("^\\S{3} | \\d{4}-\\d{2}-\\d{2}$", "", x))
}

#' Extract date or country from country-date
#'
#' Extracts the individual components from a country-date string
#'
#' @param x Character Vector
#'
#' @details Country-date strings are a string concatenation of a three
#'     letter country ID, \code{country_text_id}, and an ISO 8601
#'     date, \code{historical_date}. \code{get_date} is a vectorised
#'     function that returns the date part, converted using
#'     \code{as.Date}.
#'
#' @return Date vector
#'
#' @examples
#' get_date("SWE 1988-04-20")
#'
#' @family country_date functions
#'
#' @export
get_date <- function(x, party = FALSE) {
    assert_str(x, party)
    if (party) {
        sub("^\\S{3} \\d{6} ", "", x, perl = T) %>% as.Date 
    } else {
        sub("^\\S{3} ", "", x, perl = T) %>% as.Date
    }
}

#' @details The \code{get_text_id} function is the vectorised
#'     counterpart to \code{get_date} that returns the
#'     \code{country_text_id} components
#'
#' @examples
#' get_text_id("DEU 1951-02-23")
#'
#' @rdname get_date
#' @export
get_text_id <- function(x, party = FALSE)  {
    assert_str(x, party)
    sub("\\s\\d.*$", "",  x, perl = T) %>% unclass
}

#' Sort a vector of country-dates
#'
#' Sorts a character vector of country-dates based on the
#' \code{country_text_id} prefixes.
#'
#' @param x Character Vector
#' @param decreasing Logical indicating whether to sort by decreasing
#'     order.
#' @param na.last Logical indicating whether NAs should be appending
#'     to the end or beginning of the sorted vector.
#'
#' @details Sorts first country-date strings followed by vignette
#'     identifiers. The fact that we append vignettes to the end is
#'     simply a convenience for when working with the MM code.
#'
#' @examples
#' v <- c("USA 1920-01-01", "MEX 1902-01-01", "MEX 1900-01-01",
#'        "pilot_1_v1", "pilot_2_v1")
#' sort_text_id(v)
#'
#' @family country-date functions
#'
#' @export
sort_text_id <- function(x, decreasing = F, na.last = T, party = FALSE){
    b <- grepl("^\\S{3}\\s", x)

    # Assert first that we have valid strings and vignette tags. This
    # sort function is the only `country-date` specific function that
    # should be able to deal with vignette identifiers.
    assert_str(x[b], party)
    if (any(!is_vignette(x[!b])))
        stop("Invalid vignette string", call. = F)

    out <- c(sort(x[b], decreasing = decreasing), sort(x[!b], decreasing = decreasing))
    if (isTRUE(na.last))
        c(out, x[is.na(x)])
    else
        c(x[is.na(x)], out)
 }

#' Date to year
#'
#' Return the four digit year from a Date object.
#'
#' @param date Date or character
#'
#' @return Integer
#'
#' @examples
#' v <- as.Date(c("1900-01-01", "1901-01-01"))
#' to_year(v)
#'
#' @export
to_year <- function(date) UseMethod("to_year")

#' @export
to_year.Date <- function(date) {
    x <- rep(NA_real_, length(date))

    b <- !is.na(date)
    x[b] <- format(date[b], "%Y") %>% as.integer

    x
}

#' @export
to_year.character <- function(date) {
    x <- rep(NA_real_, length(date))

    b <- !is.na(date)
    x[b] <- substr(date, 1, 4) %>% as.integer

    x
}

#' Year to date
#'
#' @export
year_to_date <- function(v) {    
    as.Date(paste0(v, "-12-31"), format = "%Y-%m-%d")
}

#' Date to year
#'
#' @export
date_to_year <- function(date) UseMethod("to_year")






