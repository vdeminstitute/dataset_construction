#' Add empty observations by rownames or colnames
#'
#' Given a matrix and a vector of either rownames or colnames, add a
#' empty rows/columns of NA.
#'
#' @param x A matrix
#' @param missing A vector of names to expand \code{x} col/row-wise.
#'
#' @examples
#' x <- matrix(1:3, 3, 1, dimnames = list(letters[1:3], "1"))
#'
#' add_empty_rows(x, letters[4:5])
#' add_empty_cols(x, c("2", "3"))
#'
#' @export
add_empty_rows <- function(x, missing) {
    if (length(missing) == 0)
        return(x)

    rows <- as.list(stats::setNames(rep(NA, length(missing)), missing))

    out <- do.call(rbind, c(list(x), rows))
    if (!is.null(rownames(out)))
        out[order(rownames(out)),, drop = F]
    else
        out
}

#' @rdname add_empty_rows
#' @export
add_empty_cols <- function(x, missing) {
    if (length(missing) == 0)
        return(x)

    cols <- as.list(stats::setNames(rep(NA, length(missing)), missing))

    out <- do.call(cbind, c(list(x), cols))
    if (!is.null(colnames(out)))
        out[, order(colnames(out)), drop = F]
    else
        out
}

#' Rbind uneven matrices
#'
#' Rbinds two matrices of different dimensions by first filling
#' missing columns.
#'
#' @param x A matrix.
#' @param y A matrix.
#'
#' @examples
#' x <- matrix(1:9, 3, 3)
#' y <- matrix(1:3, 3, 1)
#' full_rbind(x, y)
#'
#' @export
full_rbind <- function(x, y) UseMethod("full_rbind")

#' @describeIn full_rbind S3 method for matrices
#' @export
full_rbind.matrix <- function(x, y) {
    if (is.null(colnames(x)))
        colnames(x) <- 1:ncol(x)

    if (is.null(colnames(y)))
        colnames(y) <- 1:ncol(y)

    x_cols <- colnames(x)
    y_cols <- colnames(y)

    if (!setequal(x_cols, y_cols)) {
        x <- add_empty_cols(x, setdiff(y_cols, x_cols))
        y <- add_empty_cols(y, setdiff(x_cols, y_cols))
    }

    x <- suppressWarnings(x[, order(colnames(x)), drop = F])
    y <- suppressWarnings(y[, order(colnames(y)), drop = F])

    rbind(x, y)
}

#' Add a value to a matrix
#'
#' Simple convenience function that adds a value to a given matrix and
#' returns the modified matrix.
#'
#' @param m A matrix of any type
#' @param v Value to be added to \code{m}. No attempt is made to check
#'     if the types match.
#' @param by Optional logical matrix or vector indicating which
#'     elements in \code{m} to be modified.
#'
#' @examples
#' x <- matrix(1:9, 3, 3)
#' y <- matrix(sample(c(TRUE, FALSE), 9, replace = TRUE), 3, 3)
#'
#' add_to_matrix(x, 2, by = y)
#'
#' @export
add_to_matrix <- function(m, v, by = NULL) {
    if (!is.null(by)) {
        m[by] <- m[by] + v
        m
    } else {
        m + v
    }
}

#' @export
scale_matrix <- function(m) {
    rn <- rownames(m)
    cn <- colnames(m)
    v <- scale(as.vector(m))
    m <- matrix(v, nrow = nrow(m), ncol = ncol(m), byrow = FALSE)
    if (length(rn) > 0)
        rownames(m) <- rn
    if (length(cn) > 0)
        colnames(m) <- cn
    m
}