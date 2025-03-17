#' Remove tibble classes from a data frame
#'
#' This function takes a data frame as input and removes all tibble-related
#' classes from it, including "tbl_df" and "tbl". If the input is not a
#' data frame, the function returns an error.
#'
#' @param tbl a data frame
#'
#' @return a data frame with tibble classes removed
#'
#' @export
untibble <- function(tbl) {
    stopifnot(`input is not a tibble` = "data.frame" %in% class(tbl))
    class(tbl) <- class(tbl)[!class(tbl) %in% c("tbl_df", "tbl")]
    return(tbl)
}


#' @title Column classes in a data.frame
#'
#' @description Determine all column types of data.frame 
#'
#' @details This function takes a data.frame as its single argument. 
#' It then queries all columns for their class(es) and returns a 
#' stacked matrix where the first column indicate class and the second 
#' column column name (taken from df). If a column is represented twice,
#' it means that the column has more than one class.
#'
#' @param df A data.frame
#' @return A stacked matrix with two columns
#'
#' @examples
#' time <- as.POSIXct(Sys.time(),
#'  tz = Sys.timezone(location = TRUE),
#'  origin = "1970-01-01")
#' dd <- data.frame("A" = c(NA, 1), "B" = c(NA,NA), "C" = c(1,3), "time" = time, "F" = factor(c(F)))
#' column_types(dd)
#'
#' @export
column_types <- function(df) {
    stopifnot(is.data.frame(df))
    stopifnot(nrow(df) > 0)
    stopifnot(ncol(df) > 0)
    lapply(df, class) %>%
    stack %>% 
    setNames(c("Class", "Column"))
}

#' @title Round the numeric columns of a data.frame
#'
#' @description Returns a data.frame with all numeric colums to the nth digit. 
#'
#' @details This function rounds all numeric and all POSIXct columns in \code{df}.
#' For numeric columns, it rounds to the \code{nth} digit; for POSIXct columns, it 
#' rounds to seconds (shaving off trailing seconds since Unix Epoch). Note that it rounds,
#' and not truncate. All other columns are left as is. The returned object is a data.frame.
#'
#' It checks for two conditions:
#' \enumerate{
#'   \item \code{is.numeric(column) == TRUE}, and
#'   \item \code{class(column)[1] == "POSIXct"}
#' }
#'
#' @param df A data.frame 
#' @param n Decimal position for rounding
#' @return A data.frame
#' @examples
#' df <- data.frame(
#'   nmr = as.numeric(log(10)),
#'   time = as.POSIXct(Sys.time(), tz = Sys.timezone(location = TRUE), origin = "1970-01-01"),
#'   char = LETTERS[5]
#'   )
#' 
#' print(df)                      
#' print(round_df(df = df, n = 3))
#'
#' @export
round_df <- function(df, n) {
	stopifnot(is.data.frame(df))
	# Get first class of each column
	dncls <- unname(sapply(lapply(df, class), "[[", 1))
	# Do we have numeric or POSIXct columns?
    unique_cls <- unique(dncls[dncls %in% c("numeric", "POSIXct")])
		
	if (length(unique_cls) == 0) {
		info("No columns are rounded: 0 columns are numeric or POSIXct.")
		return(df)
	}

	if ("numeric" %in% unique_cls)
		info("Numeric columns are rounded to the third decimal prior to joining.")
	if ("POSIXct" %in% unique_cls)
		info("POSIXct columns are rounded to seconds prior to joining.")
	
    df[] <- lapply(df, function(v) {
        if (is.numeric(v)) {
            round(v, n)
        } else if(class(v)[1] == "POSIXct") {
            as.POSIXct(round(v, units = "secs"))
        } else {
            v
        }
    })

    return(df)
}

#' Compare column types across multiple data frames
#'
#' This function takes one or more data frames as input and compares their
#' column types. It returns a data frame that lists the columns that are
#' common to all input data frames, along with the class of each column in
#' each data frame. If the class of a column is different across the input
#' data frames, the function returns an error and a data frame indicating
#' the columns with type clashes.
#'
#' @param ... one or more data frames
#'
#' @return a data frame listing the common columns and their classes in the
#' input data frames. If there are type clashes, the function returns an
#' error and a data frame indicating the columns with type clashes.
#'
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

#' Filter a data frame by a regular expression
#'
#' This function filters a data frame by a regular expression applied to
#' the values in a specified column. It returns a new data frame with only
#' the rows that match the regular expression.
#'
#' @param df a data frame
#' @param pattern a regular expression
#' @param x the name of the column in which to apply the regular expression
#'
#' @return a new data frame with only the rows that match the regular
#' expression in the specified column
#'
#' @export
fgrepl <- function(df, pattern, x, ...) {
    bool <- grepl(pattern, df[[x]])
    df[bool, ]
}

#' @title Compare two data.frames rowwise based on anti_joins
#' 
#' @description Returns the rowwise and bi-directional differences between
#' \code{dfnew} and \code{dfold} as a new data.frame.
#' 
#' @details This function takes two data.frames as input and compares them
#' rowwise using \code{dplyr::anti_join} in both directions (\code{dfnew != dfold} and 
#' \code{dfold != dfnew}). All columns are used for the join, which is not trivial
#' for input data.frames with columns of class double. As a partial fix, we round
#' all numeric columns to the third decimal by default prior to joining. If rounding is true,
#' columns of class POSIXct are rounded to seconds. This is done using \code{vbase::round_df}. To turn this feature off,
#' set \code{round_numeric = FALSE}. This disables roundig for both classes.
#' The function inherits \code{dplyr::anti_join} default treatment for missing values.
#' 
#' The output is a new data.frame with the number of rows equal to the number of 
#' rows identified by the joins as only being in either but not both input data.frames.
#' The default behavior is to only retrieve the intersection of columns. To instead 
#' get the union of columns, set \code{keep_cols = TRUE}. This returns the additional
#' column with values equal to NA for the data.frame that did not contain this column.
#' 
#' The column \code{dfdiff} takes on two values, - or +. The + means that \code{dfnew}
#' contains a row that is not in \code{dfold}. The opposite applies to -.
#' This functionality is best used when sorting on all columns that are of interest
#' **before** sorting on \code{dfdiff}.
#' 
#' @param dfnew The new data.frame.
#' @param dfold The old data.frame.
#' @param keep_cols Logical for keeping all columns from both. Defaults to FALSE.
#' @param round_numeric Logical for rounding all numeric columns to three digits prior to joining. If TRUE, POSIXct are rounded to seconds. Defaults to TRUE. 
#'
#' @return A new data.frame with bi-directional rowwise differences between
#' \code{dfold} and \code{dfnew}.  
#' 
#' @examples
#' df_o <- data.frame(id1 = rep(c(1L, 2L), each = 5),
#'                    id2 = rep(2006L:2010L, 2),
#'                    value = rnorm(10))
#' 
#' df_n <- rbind.data.frame(df_o[-1, 1:3],
#'                       data.frame(id1 = c(1L, 2L),
#'                       id2 = rep(2011L, 2),
#'                       value = rnorm(2)))
#'                       
#' print(df_compare(df_n, df_o), row.names = FALSE)
#' 
#' @export
df_compare <- function(dfNew, dfOld, keep_cols = FALSE, round_numeric = TRUE) {
    
    stopifnot(`Both dfnew and dfold needs to a data.frame` =
        is.data.frame(dfNew), is.data.frame(dfOld))  

    if (isTRUE(length(intersect(colnames(dfNew), colnames(dfOld))) == 0L))
        stop("There are no similar columns between between dfNew and dfOld.")
    
    if (isTRUE(any(nrow(dfNew) == 0, nrow(dfOld) == 0)))
        stop("At least one data.frame has zero rows.")
    
    # Compare common columns
    onlyInNew <- setdiff(colnames(dfNew), colnames(dfOld))
    onlyInOld <- setdiff(colnames(dfOld), colnames(dfNew))
    commonCols <- intersect(colnames(dfNew), colnames(dfOld))
    
    if (!keep_cols) {
        newCols <- commonCols
    } else {
        newCols <- union(colnames(dfNew), colnames(dfOld))
    }

    newClasses <- unlist(lapply(dfNew[, commonCols, drop = FALSE], class))
    oldClasses <- unlist(lapply(dfOld[, commonCols, drop = FALSE], class))

    if (isTRUE(any(newClasses != oldClasses))) {
		compare_column_types(
            dfNew[, commonCols, drop = FALSE], 
			dfOld[, commonCols, drop = FALSE])
		stop("Error: Column types are different!")
	}

    if (length(onlyInNew) > 0)
        info(sprintf("In new but not old: %s", toString(onlyInNew)))

    if (length(onlyInOld) > 0)
        info(sprintf("In old but not new: %s", toString(onlyInOld)))

    # Select cols
    dfNew <- selectColumns(dfNew, pickAny(newCols))
    dfOld <- selectColumns(dfOld, pickAny(newCols))
    
    # Rounding to numeric/dates    
    if (round_numeric) {
		dfNew <- round_df(df = dfNew, n = 3)
        dfOld <- round_df(df = dfOld, n = 3)
    }

    stopifnot(`Duplicated rows are not allowed` =
        no_duplicates(dfNew), no_duplicates(dfOld))

    # Rows in {left} but not in {right}
    outNew <- antiMerge(dfNew, dfOld, by = commonCols)
    outOld <- antiMerge(dfOld, dfNew, by = commonCols)

    if (nrow(outOld) > 0) outOld$change <- "-"
    if (nrow(outNew) > 0) outNew$change <- "+"

    if (nrow(outNew) == 0 & nrow(outOld) == 0) {
        info("No difference was detected")
        return(data.frame())
    }

    out <- selectColumns(
        df = rbind.data.frame(outNew, outOld),
        change,
        keep_all = TRUE)

    return(out)
}

# TODO: Support for multiple pickAny and pickAll []

#' @title Select columns of a data.frame
#' 
#' @description Returns a data.frame with only the columns specified in
#' \code{...}. The columns are evaluated in the environment of
#' \code{df}.
#' 
#' @details This function takes a data.frame as its first argument and
#' one or more column names as its remaining arguments. The column names
#' are evaluated in the environment of the data.frame. The function
#' returns a data.frame with only the columns specified in the
#' arguments.
#' 
#' @param df A data.frame
#' @param ... One or more column names
#' @param keep_all Logical for whether to keep all columns. Defaults to
#' \code{FALSE}.
#' @param drop Logical for whether to drop the data.frame if only one
#' column is selected. Defaults to \code{FALSE}.
#' 
#' @return A data.frame with only the columns specified in the
#' arguments
#' 
#' @examples
#' selectColumns(mtcars, mpg, cyl)
#' selectColumns(mtcars, mpg, cyl, keep_all = TRUE)
#' selectColumns(mtcars, mpg, drop = TRUE)
#' selectColumns(mtcars, pickAny(mpg, cyl))
#' selectColumns(mtcars, pickAll(mpg, cyl))
#' selectColumns(mtcars, pickAny(mpg, cyl), keep_all = TRUE)
#' selectColumns(mtcars, pickAll(mpg, cyl), keep_all = TRUE)
#' selectColumns(mtcars, pickAny(mpg, cyl), drop = TRUE)
#' selectColumns(mtcars, pickAll(mpg, cyl), drop = TRUE)
#'
#' @export
selectColumns <- function(df, ..., keep_all = FALSE, drop = FALSE) {

    stopifnot(is.data.frame(df))
    
    # Capture dots
    .dots <- eval(substitute(alist(...)))
    stopifnot(length(.dots) >= 1)
    
    # Capture parent of selectColumns
    parent <- callerEnvironment()
    
    # TODO: Combine several selectors
    # TODO: make object-oriented
    # TODO: Add support for grepl stuff (contains...)
    # TODO: Add support for interaction

    # Switch for calls / functions
    # e.g. SelectColumns(data, pickAny(...)) or c(...)
    if (length(.dots) == 1 && is.call(.dots[[1]])) {
        
        funcName <- deparse(.dots[[1]][[1]])

        # Switch for pickAny() / PickAll()    
        if (funcName %in% c("pickAny","pickAll") && is.function(get(funcName))) {

            out <- eval(.dots[[1]], parent)
            # Return early
            return(out)

        # Switch for c()
        } else if (funcName %in% c("c")) {

            # Rearrange .dots
            .dots <- makeSymbols(.dots)

        # Switch for union, intersect, setdiff
        } else if (funcName %in% c("union","intersect","setdiff") && is.function(get(funcName))) {
            .dots <- as.list(eval(substituteContextSymbols(.dots[[1]], parent)))
            
            if (length(.dots[[1]]) == 0) {
                stop("Result from union, intersect or setdiff is empty")
            }

        } else {
            stop(sprintf("No method implemented for `%s`",
                deparse(.dots[[1]][[1]])))
        }
    }

    # Evaluate supplied single vector to character
    # selectColumns(mtcars, vv), where e.g. vv = c("mpg", "cyl")
    if (exists(deparse(.dots[[1]]), mode = "character", envir = parent) && length(.dots) == 1) {
        .dots <- as.list(eval(substituteContextSymbols(.dots[[1]], parent)))
    }

    # Switch and/or continuation for symbols and chars
    if (all(vapply(X = .dots, FUN = is.symbol, FUN.VALUE = logical(1)))) {
        order_names <- vapply(
            X = .dots,
            FUN = deparse,
            FUN.VALUE = character(1))
    }

    # Order names for characters
    if (all(vapply(X = .dots, FUN = is.character, FUN.VALUE = logical(1)))) {
        order_names <- vapply(
            X = .dots,
            FUN = `[`,
            FUN.VALUE = character(1))
    }
    
    # Check names of symbols / characters
    stopifnot(order_names %in% names(df) & length(.dots) == length(order_names))

    # Switch to keep all or only selected columns
    if (keep_all) {
        out_names <- c(order_names, setdiff(names(df), order_names))
    } else {
        out_names <- order_names
    }

    out <- df[, out_names, drop = drop]
    stopifnot( if (!drop) is.data.frame(out) else is.vector(out) )
    
    return(out)
}

#' @title Filter rows of a data.frame
#' 
#' @description Returns a data.frame with only the rows that match the
#' expressions in \code{...}. The expressions are evaluated in the
#' environment of \code{df}.
#' 
#' @details This function takes a data.frame as its first argument and
#' one or more expressions as its remaining arguments. The expressions
#' are evaluated in the environment of the data.frame. The expressions
#' are evaluated rowwise and the function returns a data.frame with
#' only the rows that match the expressions.
#' 
#' @param df A data.frame
#' @param ... One or more expressions
#' @param combine How to combine the expressions. Either \code{all} or
#' \code{any}. Defaults to \code{all}.
#' @param includeNA Logical for whether to include NA's in the
#' expressions. Defaults to \code{FALSE}.
#' 
#' @return A data.frame with only the rows that match the expressions
#' 
#' @examples
#' df <- data.frame(a = c(NA, 1, 2), b = c("", "A", NA),
#'                 stringsAsFactors = FALSE)
#' filterRows(df, a > 1, b == "A")
#' 
#' @export
filterRows <- function(df, ..., combine = "all", includeNA = FALSE) {

    stopifnot(is.data.frame(df))
    combine <- match.arg(combine, choices = c("all", "any"))

    # Capture dots
    .dots <- eval(substitute(alist(...)))
    stopifnot(length(.dots) >= 1)
    
    # Capture parent of filterRows
    callerEnv <- callerEnvironment()

    # Substitute symbols in callerEnv
    .dots <- lapply(.dots, function(i, env) {
        substituteContextSymbols(i, env)
    }, env = callerEnv)

    # Evaluate symbols and expressions in df: output is Mat[rows, nExpressions]
    res <- evalDataFrame(df, .dots, includeNA)
    
    # Combine results
    argsList <- list(
        X = res,
        MARGIN = 1,
        FUN = switch(combine, all = quote(all), any = quote(any)))

    out <- df[do.call(apply, argsList), , drop = FALSE]
    stopifnot(is.data.frame(out))

    return(out)
}

# TODO: order individually

#' @title Order rows of a data.frame
#' 
#' @description Returns a data.frame with the rows ordered by the
#' expressions in \code{...}. The expressions are evaluated in the
#' environment of \code{df}.
#' 
#' @details This function takes a data.frame as its first argument and
#' one or more expressions as its remaining arguments. The expressions
#' are evaluated in the environment of the data.frame. The expressions
#' are evaluated rowwise and the function returns a data.frame with
#' the rows ordered by the expressions.
#' 
#' @param df A data.frame
#' @param ... One or more expressions
#' @param naLast Logical for whether to put NA's last. Defaults to
#' \code{TRUE}.
#' @param decreasing Logical for whether to sort in decreasing order.
#' Defaults to \code{FALSE}.
#' @param method The sorting method. Either \code{auto}, \code{shell},
#' or \code{radix}. Defaults to \code{auto}.
#' @param foreground Logical for whether to move the ordering variables
#' to the front of the data.frame. Defaults to \code{FALSE}.
#' 
#' @return A data.frame with the rows ordered by the expressions
#' 
#' @examples
#' df <- data.frame(a = c(NA, 1, 2), b = c("", "A", NA),
#'                stringsAsFactors = FALSE)
#' organiseRows(df, a, b)
#' 
#' @export
organiseRows <- function(df, ..., naLast = TRUE, decreasing = FALSE, 
    method = c("auto", "shell", "radix"), foreground = FALSE) {

    stopifnot(is.data.frame(df))
    method <- match.arg(arg = method, choices = method)
    foreground <- match.arg(
        arg = as.character(foreground),
        choices = c("TRUE", "FALSE"))
    
    # Capture dots
    .dots <- eval(substitute(alist(...)))
    
    # Make symbols into names
    order_names <- vapply(
        X = .dots,
        FUN = deparse,
        FUN.VALUE = character(1))
    stopifnot(order_names %in% names(df))

    # Evaluate variables that are to order the data.frame
    organisers <- lapply(
        X = .dots,
        FUN = function(expr, env) {
            eval(
                expr = expr,
                envir = env,
                enclos = parent.frame())
                }, env = as.environment(df))

    # Collect arguments
    args_list <- c(
        organisers,
        na.last = naLast,
        decreasing = decreasing,
        method = method)

    # Switch to move ordering variables to the front
    if (foreground) {
        out_order_names <- c(order_names, setdiff(names(df), order_names))
    } else {
        out_order_names <- names(df)
    }

    out <- df[do.call(order, args_list), out_order_names, drop = FALSE]
    return(out)
}

#' Use the first row of a data frame as column names
#'
#' This function takes a data frame as input and sets the names of its columns
#' to the values in the first row of the data frame. It then removes the first
#' row from the data frame and returns the modified data frame.
#'
#' @param df a data frame
#'
#' @return a data frame with the first row as column names and the first row
#' removed
#'
#' @export
first_row_to_names <- function(df) {
    stopifnot(is.data.frame(df))

	names(df) <- df[1, , drop = TRUE]
	df <- df[-1, , drop = FALSE]
	return(df)
}

#' @title Anti join two data.frames
#' @description Returns the rows in \code{x} that are not present in \code{y}
#' 
#' @param x a data.frame
#' @param y a data.frame
#' @param by a character vector of column names to join by. Defaults to all common columns.
#' 
#' @return a data frame with only the rows that are in the first data frame but not in the second data frame, given a set of keys
#' 
#' @examples
#' x <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
#' y <- data.frame(a = c(1, 2, 3), b = c(4, 5, 7))
#' antiMerge(x, y)
#' antiMerge(x, y, by = "a")
#' antiMerge(x, y, by = c("a", "b"))
#' antiMerge(x, y, by = "b")
#' 
#' @export
antiMerge <- function(x, y, by) {
    
    stopifnot(vapply(list(x,y), is.data.frame, logical(1)))

    if (missing(by)) {
        by <- intersect(colnames(x), colnames(y))
        if (length(by) == 1 && allNA(x[[by]])) {
            stop("by-column is all NA")
        }
        info(sprintf("Joining by: %s", toString(by)))
    }

    if (all(by %in% colnames(x)) && all(by %in% colnames(y))) {

        xRownamesClass <- class(attributes(x)$row.names)
        xRownames <- as.vector(rownames(x), mode = xRownamesClass)
        x[[".antiMergeId"]] <- seq(from = 1, to = nrow(x))

        # If all are NA, then we cannot use that column for interaction directly
        inner <- merge(x=x, y=y, by=by, all.x=FALSE, all.y=FALSE)
        out <- x[!x[[".antiMergeId"]] %in% inner[[".antiMergeId"]], ]

        outId <- out[[".antiMergeId"]]
        out[[".antiMergeId"]] <- NULL
        rownames(out) <- xRownames[outId]
        stopifnot(is.data.frame(out))

    } else {
        stop("by-columns not present in at least one data.frame")
    }

    return(out)
}