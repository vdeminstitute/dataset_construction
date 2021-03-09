#' Show duplicates from a data.frame
#'
#' Show the duplicates of a data.frame based on spcified columns
#' The output data.frame shows the original value and the duplicates.
#' @param df A data.frame.
#' @param cols A character vector of column names
#'
#' @export
duplicates <- function(df, cols) {
    rbind(df[duplicated(df[, cols]), ],
          df[duplicated(df[, cols], fromLast = T), ])
}

#' Test for duplicates in a data.frame
#'
#' @export
no_duplicates <- function(df, cols) {
    nrow(duplicates(df, cols)) == 0L
}


#' @export
check_df <- function(df) {
    if (any(grepl("[.](x|y)$", names(df))))
        stop("column names have .x or .y!")
    if (dplyr::is.grouped_df(df))
        stop("This is a grouped tibble...")
    if (dplyr::is_tibble(df))
        stop("This is a tibble...")
    df
}

#' Check columns of a data.frame or matrix for strange values
#'
#' Goes through column by column to check for NA's, Dates out of
#' range, empty strings etc.
#'
#' @param df A data.frame or a matrix object
#'
#' @examples
#' df <- data.frame(a = c(NA, 1, 2), b = c("", "A", NA),
#'                  stringsAsFactors = FALSE)
#' check_columns(df)
#'
#' @export
check_columns <- function(df) UseMethod("check_columns")

#' @export
check_columns.data.frame <- function(df) {
    if (!is.data.frame(df))
        stop("Object checked is no data.frame!")

    # Determine functions to check for each cell by type, must
    # evaluate to logical
    char_funs <- c(is.na,
                   function(x) x == "",
                   function(x) grepl("\n", x, fixed = T),
                   function(x) grepl("\r", x, fixed = T),
                   function(x) trimws(x, which = "both") != x)
    num_funs <- c(is.na,
                  is.nan,
                  function(x) x < 0)
    date_funs <- c(is.na,
                   function(x) x > Sys.Date(),
                   function(x) x < as.Date(c("17880101"), format = '%Y%m%d'))
    factor_funs <- c(function(x) T,
                     is.na)
    other_funs <- c(is.na)

    # Name functions for output
    names(char_funs) <- c("NA", "''", "\\n", "\\r", "leading or trailing white spaces")
    names(num_funs) <- c("NA", "NaN", "<0")
    names(date_funs) <- c("NA", "> today", "< 1788-01-01")
    names(factor_funs) <- c("type!", "NA")
    names(other_funs) <- c("NA")

    # Check each cell in each column
    check_cols_f <- function(df, funs, n = names(funs), type) {
        if (nrow(df) == 0)
            return(NULL)

        invisible(lapply(seq_along(funs), function(x, n, f) {
            res <- Filter(isTRUE, unlist(lapply(lapply(df, f[[x]]), any)))

            if (length(res) > 0) {
                sprintf("Column(s) %s have: %s %s",
                        paste(names(res), collapse = ", "),
                        type,
                        n[x]) %>% warn
            }
        }, n = n, f = funs))
    }

    # Split data.frame into types
    df_char <- df[, lapply(df, class) == "character", drop = F]
    df_num <- df[, lapply(df, class) == "numeric", drop = F]
    df_date <- df[, lapply(df, class) == "Date", drop = F]
    df_factor <- df[, lapply(df, class) == "factor", drop = F]
    df_other <- df[, !colnames(df) %in% c(colnames(df_char), colnames(df_num),
                                         colnames(df_date), colnames(df_factor)), drop = F]

    # Call sub functions on each sub data.frame
    check_cols_f(df_num, num_funs, type = "numeric")
    check_cols_f(df_char, char_funs, type = "character")
    check_cols_f(df_date, date_funs, type = "Date")
    check_cols_f(df_factor, factor_funs, type = "factor")
    check_cols_f(df_other, other_funs, type = "Other")
}

#' @export
check_columns.matrix <- function(df)
    check_columns.data.frame(as.data.frame(df, stringsAsFactors = F))




# Work in progress
#############################################################################
#' @export
check_codebook <- function(df) {
    # datarelease (all follow format?)

    # crosscoder_aggregation
    # responses (any missing?)
    # class (any missing?)
    # cb_section (any missing?)
    # conthistmerge: hist-only, no-merge, merge_conflict, merge (only v3 rest "")
    # histmerged: (v2 and v3)
    # hist_outside_coding (v3 rest FALSE)
    # cont_outside_coding (v2 rest FALSE)
    # date_specific (report)
    # cleaning (report)

    # report about status
}

#' @export
check_qtable <- function(df) {



}