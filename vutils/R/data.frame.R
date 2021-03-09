#' Transform tibble to data.frame
#'
#' @description
#' Remove tibble attributes
#'
#' @param df A tibble
#'
#' @return A data.frame
#'
#' @export
no_tibble <- function(df) {
    if (any(col_types(df) == "factor"))
        stop("A column is of type factor!")
    as.data.frame(df, stringsAsFactors = FALSE)
}

#' Remove tibble class attritubes from data.frame
#' @export
untibble <- function(tibtib) {
    stopifnot("data.frame" %in% class(tibtib))
    class(tibtib) <- class(tibtib)[!class(tibtib) %in% c("tbl_df", "tbl")]
    return(tibtib)
}


#' Determine column types of a data.frame
#'
#' @description
#' Determine column types of data.frame
#'
#' @return Named character vector of coulmn types.
#'
#' @export
column_types <- function(df) {
    lapply(df, class) %>% unlist
}


#' Round the numeric columns of a data.frame
#'
#' @param df A data.frame
#' @param n Decimal position for rounding
#' @return A data.frame
#' @export
round_df <- function(df, n) {
    df[] <- lapply(df, function(v) {
        if (is.numeric(v)) {
            round(v, n)
        } else {
            v
        }
    })
    return(df)
}

#' @export
compare_column_types <- function(...) {
    ll <- list(...)
    # Works with several objects or list of objects
    if (length(ll) == 1)
        ll <- ll[[1]]
    nn <- names(ll)
    # If function call is without names then create them.
    if (is.null(nn)) {
        nn <- 1:length(ll) %>% as.character
        names(ll) <- nn
    }
    
    out <- Map(function(df, nn) {
        outdf <- data.frame(names = names(df))
        outdf[[nn %^% "_class"]] <- lapply(df, 
                        function(v) paste(class(v), collapse = ",")) %>% unlist
        return(outdf)
    }, df = ll, nn = names(ll))

    common_cols <- lapply(out, function(df) {
        df$names
    }) %>% unlist %>% table %>% as.data.frame(stringsAsFactors = FALSE) %>% 
        dplyr::filter(Freq > 1) %>% {.[, 1]}

    res <- Reduce(partial(dplyr::full_join, by = "names"), out) %>%
        dplyr::filter(names %in% common_cols)

    no_clash <- apply(res[, -1], 1, function(v) {length(unique(na.omit(v))) == 1})
    
    if (nrow(res[!no_clash, ]) > 0) {
        info("There are column type clashes")
        return(res[!no_clash, ])
    } else {
        info("There are no column type clashes.")
    }
    
}


