

#' Remove new line characters
#'
#' Replaces new line characters in a character vector with a blank space
#'
#' @param v A character vector
#'
#' @examples
#' v = c("Mr. Smith \\n goes home")
#' rm_newline(v)
#'
#' @export
rm_newline <- function(v) {
    out <- trimws(gsub("\r|\n", " ", v))
    return(out)
}

#' Swap single comma deliminated strings
#'
#' \code{comma_swap} is meant to transform comma deliminated country
#' names like "Korea, South" to their canonical name, "South Korea".
#'
#' @param v \code{CharacterVector}
#'
#' @section Warning: \code{comma_swap} assumes only a single comma in
#'     each string and will issue an error if there aren't exactly two
#'     elements after splitting on ','.
#'
#' @return \code{CharacterVector}
#'
#' @examples
#' comma_swap(c("Korea, South", "Korea, North"))
#'
#' @export
comma_swap <- function(v) {
    idx <- grepl(",", v)

    if (!any(idx)) return(v)
    
    v[idx] <-
        vapply(
            X = strsplit(x = v[idx], split = ","),
            FUN = function(n) {
                if (length(n) != 2) stop("Only one comma per string.")

                out <- trimws(paste(rev(n), collapse = " "))
                return(out)
            },
            FUN.VALUE = character(1) )

    return(v)
}

#' @export
clean_vname <- function(dirty_vname) {
    return(gsub("_\\d{6}.rds", "", basename(dirty_vname)))
}