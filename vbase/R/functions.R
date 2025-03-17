#' @title Partial function application
#'
#' @description This function creates an enclosure for the call given by \code{fn} and the arguments given by \code{...}. The returned function can be called with additional arguments, which will be appended to the call.
#'
#' @param fn A function as symbol or character.
#' @param ... Arguments to be applied.
#'
#' @examples
#' x <- c(1, 2, NA, 4)
#' f <- partial(mean, na.rm = TRUE)
#' ls.str(environment(f))
#' f(x)
#'
#' @export
partial <- function(fn, ...) {

    if (is.character(fn)) {
        fn <- as.symbol(fn)
    }

    stopifnot(is.function(fn))
    dots <- list(...)

    outFun <- function(...) {
        do.call(fn, c(dots, list(...)))
    }

    return(outFun)
}