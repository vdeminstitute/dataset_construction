#' Add n empty rows to a character vector
#'
#' @export
append_empty_rows <- function(s, n = 1) {
    c(s, rep("\n", n))
}

#' @export
append_newline <- function(s) {
    paste0(gsub("\n", "", s, fixed = T), "\n")
}

#' @export
combine_string <- function(s) {
    paste(s, collapse = "")
}

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
    gsub("\r|\n", " ", v) %>% trimws
}

#' Multi-line string
#' 
#' @export
mlstr <- function(v) {
    # Remove end of line or tab
    gsub("[\r\n\t]", " ", v) %>% 
    # Remove double spaces
    gsub("[[:space:]]+", " ", .) %>%
    # Trim white space at beginning and end
    trimws
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
    v[idx] <- strsplit(v[idx], ",") %>%
        lapply(function(n) {
            if (length(n) != 2)
                stop("Invalid number of commas in string")

            rev(n) %>% paste(collapse = " ") %>% trimws
        }) %>% unlist

    v
}

#' Prepare string for postgres query
#'
#' @export
pg_text <- function(v) {
    cat(paste0("'", paste0(v, collapse = "','"), "'"), sep = "\n")
}