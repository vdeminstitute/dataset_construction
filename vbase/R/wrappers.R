#' Wrapper around merge with all.x = TRUE and a warning if the x object get additional rows
#'
#' @export
leftMerge <- function(x, y, ...) {

    .call <- match.call()
    .call[1] <- call("merge")

    .dots <- list(...) 
    #.dots <- eval(substitute(alist(...())))
    for (i in names(.dots)) {
        .call[i] <- .dots[[i]]
    }

    # Left join
    .call["all.x"] <- TRUE    

    # Evaluate merge
    out <- eval(expr = .call)    

    if (nrow(x) != nrow(out)) {
        warn(sprintf("%d difference in rows post-merge", nrow(x) - nrow(out)))
    }

    # Merge message

    return(out)

}


#' Wrapper around View with message for zero rows
#'
#' @export
view <- function(x, title = "") {

    if (!is.data.frame(x)) {
        if (is.vector(x)) {
            vbase::info("Converting input vector to a data.frame and adding index.")
            x <- data.frame(vec = x, index = seq_along(x))
        }
    }

    stopifnot(is.data.frame(x))

    if (nrow(x) == 0L) {
        vbase::info("The output has zero rows.")
    }

    vbase::info(sprintf("Showing %d rows", nrow(x)))
    View(x, title)
    return(invisible())
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
