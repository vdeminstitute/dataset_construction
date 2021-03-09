#' Formatted logging
#'
#' Outputs a formatted log message.
#'
#' @param text Text, string to be printed.
#' @param type Type of message, either WARN or INFO.
#'
#' @examples
#' log_msg("This is a warning", type = "WARN")
#'
#' @export
log_msg <- function(text, type = c("INFO", "WARN", "ERROR")) {
    level <- match.arg(type)

    if (!exists("VARNAME"))
        VARNAME <- "none"

    if (class(text) == "numeric")
        text <- prettyNum(text, big.mark = ",", scientific = FALSE)

    if (!interactive()) {
        argv <- paste(commandArgs(), collapse = " ")
        script_call <- sub("^.*--file=(.*)(\\s|${1}?)(.*)", "\\1", argv) %>%
            basename

        cat(sprintf("[%s] [%s] [PID: %s] [%s] [%s] %s\n", script_call, 
            VARNAME, Sys.getpid(), Sys.time(), level, text), sep = "")

    } else {
        s <- sprintf("[%s] [%s] %s\n", Sys.time(), level, text)

        word_cols <- c("INFO" = "#FE43C3",
                       "WARN" = "#FE43C3",
                       "Reading" = "#FE43C3",
                       "Writing" = "#FF0000",
                       "file" = "#FE43C3")

        color_cat(s = s,
                  words = names(word_cols),
                  front_colors = word_cols,
                  sep = "")
    }
}

#' Warning message
#'
#' Convenience wrapper around `log_msg` to output formatted warning
#' messages.
#'
#' @param ... Arguments passed to \code{\link{log_msg}}
#'
#' @examples
#' warn("This is a warning")
#'
#' @export
warn <- function(...) log_msg(type = "WARN", ...)

#' Information message
#'
#' Convenience wrapper around `log_msg` to output formatted info
#' messages.
#'
#' @param ... Arguments passed to \code{\link{log_msg}}
#'
#' @examples
#' info("This is an information message")
#'
#' @export
info <- function(...) log_msg(type = "INFO", ...)

#' \code{try-error}
#'
#' Convenience functions for dealing with error object results from a
#' failed \code{try} expression.
#'
#' @name try_error
NULL

#' @describeIn try_error Indicates whether an object is a
#'     \code{try-error}.
#'
#' @param x Object to test
#'
#' @examples
#' x <- try(.unknown_function(), silent = TRUE)
#' is.try_error(x)
#'
#' @export
is.try_error <- function(x) {
    inherits(x, "try-error")
}

#' @describeIn try_error Returns the error message associated with a
#'     try-error object.
#'
#' @param e \code{try-error} object
#'
#' @export
get_errmsg <- function(e) {
    if (is.try_error(e))
        attr(e, "condition")$message
}
