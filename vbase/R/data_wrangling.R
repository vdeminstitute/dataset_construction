#' @title Apply a function per group and combine results
#'
#' @description Simple wrapper around the conventional split-lapply-unsplit pattern
#' that, unlike \code{\link{by}}, returns a combined object.
#'
#' @param x An R object, either \code{data.frame} or \code{matrix}
#' @param factors Factor or list of factors passed to
#'     \code{\link{split}} and \code{\link{unsplit}}
#' @param fn Function to apply to each group
#' @param mc.cores Number of cores to use as parallel jobs.
#' @param ... Additional arguments passed to
#'     \code{\link[parallel]{mclapply}}
#'
#' @section Warning: \code{by_split} assumes that the subsets returned
#'     by the function \code{f} are of the same class as the
#'     original object.
#'
#' @return Object of same class as \code{x}.
#'
#' @examples
#' df <- data.frame(x = c(1, 1, 2, 2), y = c(1, NA, NA, 4))
#' by_split(df, df$x, locf)
#'
#' @export
by_split <- function(x, factors, fn, mc.cores = 1, ...) {
    if (class(fn) != "function")
        stop("Invalid function")

    UseMethod("by_split")
}

#' @export
by_split.matrix <- function(x, factors, fn, mc.cores = 1, ...) {
    ll <- parallel::mclapply(
        X = split.data.frame(x = x, f = factors),
        FUN = fn,
        mc.cores = mc.cores,
        ...)

    mc_assert(ll)
    names(ll) <- NULL

    out <- do.call(rbind, ll)
    return(out)
}

#' @export
by_split.data.frame <- function(x, factors, fn, mc.cores = 1, ...) {
    ll <- parallel::mclapply(
        X = split(x = x, f = factors),
        FUN = fn,
        mc.cores = mc.cores,
        ...)

    mc_assert(ll)
    names(ll) <- NULL

    if (sum(vapply(ll, nrow, numeric(1))) == nrow(x))
        out <- unsplit(value = ll, f = factors)
    else
        out <- do.call(what = rbind, args = ll)

    return(out)
}




#' @title wide-to-long reshaping of a data.frame
#' @description Reshape a data.frame from wide format to long format.
#' @details This function first converts df to a data.table and then uses
#' data.table::melt with aliased arguments. It pivots all
#' variables that are not specified in \code{id_vars}. Hence, measure.vars
#' gets all columns that are not specified in id_vars. A message is printed
# 'comparing the old and new dimensions.
#' @param df A data.frame.
#' @param id_vars A character vector of identifier column names that should not be reshaped. Alias for \code{id.vars}.
#' @return A data.frame with two new columns additional to \code{id_vars}, 
#' \code{variable} for former column names, and \code{value} for the corresponding values.
#' @examples
#' df <- data.frame(
#'		country_text_id = c("DEU", "DEU", "FRA", "FRA"),
#'		year = c(2001, 2002, 2001, 2002),
#'		v2clacfree = c(1, 1, 2, 2),
#'		v2blatype = c(3, 3, 4, 4))
#' wide_to_long(df, id_vars = c("country_text_id", "year"))
#' @export
wide_to_long <- function(df, id_vars = NULL) {
    
    stopifnot(is.data.frame(df))
    
    dfLong <- as.data.frame(
        data.table::melt(
            data = data.table::as.data.table(df),
            id.vars = id_vars,
            measure.vars = colnames(df)[!colnames(df) %in% id_vars],
            variable.factor = FALSE)
    )

    info(sprintf("From wide dimensions: [%d, %d]", nrow(df), ncol(df)))
    info(sprintf("To long dimesions: [%d, %d]", nrow(dfLong), ncol(dfLong)))

    return(dfLong)
}

#' @title long-to-wide reshaping of a data.frame
#' @description Reshape a data.frame from long format to wide format.
#' @details This function first converts df to a data.table and then uses
#' data.table::dcast with aliased arguments. It reshapes to wide based on three arguments:
#' \code{id_vars} encode the post-reshaping identifying variables, which will identify each row,
#' \code{id_var} is the current identifying variable, \code{value_var} should be a character
#' string with the column that holds the values. A message is printed comparing
#' the old and new dimensions.
#'
#' @param df A data.frame.
#' @param id_vars A character vector of identifier column names that should not be reshaped.
#' @param id_var A String with the name of the current identifier column whose data will be 
#'	 transformed to column names.
#' @param value_var A String with the name of the current value column.
#' @return A data.frame with new columns, one for each unique value in \code{id_var}.
#' @examples
#' df <- data.frame(
#'		name = c("v2clacfree", "v2clacfree", "v2blatype", "v2blatype"),
#'		country_text_id = c("DEU", "DEU", "FRA", "FRA"),
#'		year = c(2001, 2002, 2001, 2002),
#'		value = c(1, 1, 2, 2)) 
#' long_to_wide(df, id_vars = c("country_text_id", "year"),
#'				id_var = "name", value_var = "value")
#' @export
long_to_wide <- function(df, id_vars, id_var, value_var) {
    
    stopifnot(is.data.frame(df))

    dfWide <- as.data.frame(
        data.table::dcast(
            data = data.table::as.data.table(df),
            formula = as.formula(paste0(paste(id_vars, collapse = "+"), "~", id_var)),
            value.var = value_var)
    )

    info(sprintf("From long dimesions: [%d, %d]", nrow(df), ncol(df)))
    info(sprintf("To wide dimensions: [%d, %d]", nrow(dfWide), ncol(dfWide) ))

    return(dfWide)
}


