#' Wrapper aroung dplyr left_join with warning for inserted rows
#'
#'
#' @export
left_join_ <- function(...) {
    dots <- list(...)
    df <- dplyr::left_join(...)
    if (nrow(df) > nrow(dots[[1]]))
        warn(as.character(nrow(df) - nrow(dots[[1]])) %^%
        " rows were inserted by left_join!")
    df
}


#' Wrapper around View with message for zero rows
#'
#' I should create methods for vectors
#' @export
view <- function(df, title = "df") {
    if (nrow(df) == 0L)
        return(message("The output has zero rows."))
    print(nrow(df))
    View(df, title)
}

#' Wrapper around table with option to always show NAs
#'
#' @export
table_ <- partial(table, useNA = "always")


#' Wrapper around grep with option to always show values
#'
#' @export
grep_ <- partial(grep, value = T)

#' Wrapper around head and view
#'
#' @export
hview <- function(df, x = 10) {
    view(utils::head(df, x))
}


#' Wrapper around dplyr filter (needs to be fixed)
#'
#' @export
fgrepl <- function(df, pattern, x, ...) {
    bool <- grepl(pattern, df[[x]])
    df[bool, ]
}

#' @export
str_ <- function(obj, max.level = 2, give.attr = FALSE, ...) {
    cat("Class(es): ")
    cat(class(obj))
    cat("\n\nObject:\n")
    cat(str(unclass(obj), max.level = max.level, give.attr = give.attr, ...))
    cat("\nType: \n")
    cat(typeof(obj))
    cat("\n\nAttributes:\n")
    cat(str(attributes(obj)))
}
