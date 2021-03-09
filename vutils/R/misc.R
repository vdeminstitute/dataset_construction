#' Partial function application
#'
#' Partially applies a variable number of arguments to a given function.
#'
#' @param fn A function.
#' @param ... Arguments to be applied.
#'
#' @examples
#' x <- c(1, 2, NA, 4)
#' f <- partial(mean, na.rm = TRUE)
#' f(x)
#'
#' @export
partial <- function(fn, ...) {
    dots <- list(...)

    force(fn)
    function(...) do.call(fn, c(dots, list(...)))
}

#' Evaluate an expression with substitution
#'
#' \code{evalMacro} takes an unevaluated expression and an environment
#' to first substitute the specified objects and then evaluate the
#' resulting expression. The function \code{\link{bquote}} is used for
#' substitution, see the help page for substitution syntax.
#'
#' @param expr Expression
#' @param env Environment used for substitution.
#'
#' @details We use \code{\link{bquote}} instead of
#'     \code{\link{substitute}} to force an explicit syntax
#'     highlighting which objects should be substituted. The resulting
#'     expression is then evaluated within the context of the parent
#'     environment (see: \code{\link{parent.frame}}).
#'
#' @examples
#' x <- 1
#' expr <- quote(.(a) + 1)
#'
#' evalMacro(expr, list(a = x))
#'
#' df <- data.frame(x = c(1, 2, 3))
#' transform(df, y = evalMacro(expr, list(a = x)))
#'
#' @export
evalMacro <- function(expr, env = parent.frame()) {
    x <- do.call(bquote, list(expr, env))
    eval(x, parent.frame())
}


#' Check if arguments are \code{identical}
#'
#' Given an initial function argument, \code{all_identical} loops
#' through the remaining arguments checking that they are
#' \code{\link{identical}}.
#'
#' @param ... Objects to check
#'
#' @return A single boolean indicating whether all of the provided
#'     arguments are identical to each other.
#'
#' @examples
#' all_identical(list("a", "b"), list("a", "b"))
#'
#' all_identical("Not", "Equal")
#'
#' @export
all_identical <- function(...) {
    if (missing(...))
        stop("Missing arguments", call. = F)

    args <- list(...)

    if (length(args) < 2)
        return(TRUE)

    bools <- logical(length(args) - 1)
    for (i in 2:length(args))
        bools[i - 1] <- identical(args[[1]], args[[i]])

    all(bools)
}

#' Set union for variable number of arguments
#'
#' \code{s_union} returns the unique elements from a set
#' \code{\link{union}} between all function arguments.
#'
#' @param ... Variable number of arguments of any type.
#'
#' @section Warning: The resulting output object will not inherit the
#'     user-defined attributes of the input objects.
#'
#'     Also, as a further note, input objects are expected to be of
#'     the same type and are therefore exposed to R's normal system of
#'     type coercion.
#'
#' @examples
#' s_union(letters[1:3], letters[2:4], letters[3:5])
#'
#' @export
s_union <- function(...) {
    if (missing(...))
        stop("Missing arguments", call. = F)

    args <- list(...)

    if (length(args) < 2)
        return(args[[1]])

    unique(do.call(c, args))
}

#' Copy column 900 times
#'
#' \code{make_900cols} returns the data frame ready for BFA.
#' The first column contains the identifiers.
#'
#' @param df Input data.frame that should contain the column of interest 
#' and case identifiers: country_text_id and historical date
#' 
#' @param var Variable to be copied 900 times
#'
#' @export
make_900cols <- function(df, var, interpolate = FALSE) {
    stopifnot(("historical_date" %in% names(df) | "country_text_id" %in% names(df)) & var %in% names(df))

    df <- df[, c("country_text_id", "historical_date", var)]
    df <- df[!grepl("^A_|^B_", df[["historical_date"]]),]

    if(interpolate) {
    df %<>%
        dplyr::group_by(country_text_id) %>%
        dplyr::arrange(historical_date) %>%
        dplyr::group_by(gap_idx = create_idx(historical_date), add = TRUE) %>%
        dplyr::mutate_at(dplyr::vars(var), locf) %>%
        dplyr::ungroup()    
    }
     
    df %<>%
        dplyr::mutate(country_dates = paste(country_text_id, historical_date, sep = " "))

    df_no_na <- df[!is.na(df[, var]),]

    cnames <- c("", paste0("V", 1:900))

    df_copied <- df_no_na[, c("country_dates", var)] %>%
        cbind(., replicate(899, .[, var]))
    names(df_copied) <- cnames

    return(df_copied)
}