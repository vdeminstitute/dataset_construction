#' \code{try-error}
#'
#' Convenience functions for dealing with error object results from a
#' failed \code{try} expression.
#'
#' @name try_error
NULL

#' @describeIn try_error Indicates whether an object is a
#'     \code{try-error}.
#'
#' @param x Object to test
#'
#' @examples
#' x <- try(.unknown_function(), silent = TRUE)
#' is.try_error(x)
#'
#' @export
is.try_error <- function(x) {
    inherits(x, "try-error")
}

#' @describeIn try_error Returns the error message associated with a
#'     try-error object.
#'
#' @param e \code{try-error} object
#'
#' @export
get_errmsg <- function(e) {
    if (is.try_error(e))
        attr(e, "condition")$message
}

#' Check parallelized results for errors
#'
#' Given a list of outputs from \code{\link[parallel]{mclapply}} or
#' \code{\link[parallel]{mcMap}}, asserts whether any errors were
#' encountered.
#'
#' @param ll List of results
#'
#' @details Currently, no attempt is made to ascertain the location
#'     within the results list of any errors. Feel free to properly
#'     extend this function.
#'
#' @examples
#' \dontrun{
#' ll <- parallel::mclapply(1:3, sdf)
#' mc_assert(ll)
#' }
#'
#' @export
mc_assert <- function(ll) {
    if (!is.list(ll))
        ll <- as.list(ll)

    err <- Filter(is.try_error, ll) %>% unique

    if (length(err) > 0) {
        vapply(err, get_errmsg, character(1)) %>%
            paste(collapse = "\n") %>%
            stop(call. = F)
    }
}



