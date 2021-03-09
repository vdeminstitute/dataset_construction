#' or operator
#'
#' Returns the \code{lhs} object unless it is unset, NULL, NA, or has
#' a \code{length} of 0 in which case the \code{rhs} object is
#' returned. Useful for specifying default or fallback values for
#' variables or values.
#'
#' @param lhs An object of any type.
#' @param rhs An object of any type.
#'
#'
#' @details The or operator checks the \code{lhs} as a whole ---
#'     \emph{i.e.}, unlike \code{ifelse} it does not replace
#'     individual elements matching the stated conditions with the
#'     \code{rhs} object counterparts. This also means that the
#'     \code{rhs} object does not have to be of the same type as the
#'     \code{lhs} object.
#'
#' @examples
#' c(1, 2, NA, 4) %||% c("a", "b", "c", "d")
#' numeric(0) %||% "V-Dem"
#' unknown_variable %||% 42
#'
#' @rdname or-op
#' @name or-op
#' @export
`%||%` <- function(lhs, rhs) {
    r <- try(get0(deparse(quote(lhs))), silent = T)
    if (class(r) == "try-error")
        return(rhs)

    if (is.null(lhs) || (is.vector(lhs) && all(is.na(lhs))) || length(lhs) == 0)
        rhs
    else
        lhs
}

#' concatenate operator
#'
#' Concatenates two strings together using the \code{paste0}
#' function. \emph{No attempt is made to collapse either vector
#' argument}.
#'
#' @param lhs A character vector.
#' @param rhs A character vector.
#'
#' @section Warning: Because the concatenate operator makes no attempt
#'     to unwind potential recursive calls, there are much more
#'     efficient ways of concatenating large numbers of character
#'     vectors together.
#'
#' @examples
#' "Hello " \%^\% "World!"
#' c("a", "b") \%^\% c("c", "d")
#' "foo" \%^\% c("a", "b")
#'
#' @rdname concatenate-op
#' @export
`%^%` <- function(lhs, rhs) {
    paste0(lhs, rhs)
}

#' @export
`%~%` <- function(lhs, rhs) {
    grepl(rhs, lhs)
}

#' @export
`%!~%` <- function(lhs, rhs) {
    !grepl(rhs, lhs)
}

#' @export
`%contains%` <- function(lhs, rhs) {
    rhs %in% lhs
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %$%
#' @export
magrittr::`%$%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @importFrom magrittr %T>%
#' @export
magrittr::`%T>%`
