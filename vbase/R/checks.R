#' @title Show duplicates from a data.frame
#'
#' @description Show the duplicate rows of a data.frame based on specified columns.
#' 
#' @param df A data.frame.
#' @param cols A character vector of column names.
#' @param keep_all A logical scalar that decides if all columns should be returned.
#' @param ... Accepts a named vector of additional columns to include in the output.
#'
#' @return A data.frame with the duplicate rows.
#' @export
duplicates <- function(df, cols, keep_all = FALSE, ...) {
	
    stopifnot(is.data.frame(df))
    
    if (missing(cols)) {
		cols <- names(df)
	}

    stopifnot(is.character(cols))
    stopifnot(all(cols %in% names(df)))

    dup_df <- df[, cols, drop = FALSE]
    dup1_lgl_vec <- duplicated(dup_df)
    dup2_lgl_vec <- duplicated(dup_df, fromLast = TRUE)

    dots <- list(...)

    if (keep_all) {
        out_cols <- names(df)
    } else {
        out_cols <- cols
        if (!is.null(dots)) {
            if ("keep_additional" %in% names(dots)) {
                keep_additional <- dots$keep_additional
                stopifnot(is.character(keep_additional))
                stopifnot(keep_additional %in% names(df))
                out_cols <- c(out_cols, keep_additional)
            }
        }
    }

    out <- rbind(
        df[dup1_lgl_vec, out_cols, drop = FALSE],
        df[dup2_lgl_vec, out_cols, drop = FALSE])
    stopifnot(is.data.frame(out))

    return(out)
}

#' @title Test for duplicates in a data.frame
#' 
#' @description Check for duplicate rows of a data.frame based on specified columns.
#' @param df A data.frame.
#' @param cols A character vector of column names.
#' 
#' @return Logical vector of length 1.
#' @export
no_duplicates <- function(df, cols) {

    stopifnot(is.data.frame(df))
    
	if (missing(cols)) {
		cols <- names(df)
	}

    stopifnot(is.character(cols))
    stopifnot(all(cols %in% names(df)))

    out <- nrow(duplicates(df, cols)) == 0L
    stopifnot(isTRUE(out) | isFALSE(out))
    return(out)
}


