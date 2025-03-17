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
        script_call <- basename(sub("^.*--file=(.*)(\\s|${1}?)(.*)", "\\1", argv))

        s <- sprintf("[%s] [%s] [PID: %s] [%s] [%s] %s\n", script_call, 
            VARNAME, Sys.getpid(), Sys.time(), level, text)

    } else {
        s <- sprintf("[%s] [%s] %s\n", Sys.time(), level, text)
    }
	
	if (level == "INFO")
		message(s)
	if (level == "WARN")
		warning(s, call. = FALSE)
	if (level == "ERROR")
		stop(s, call. = FALSE)

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

#' Error message
#'
#' Convenience wrapper around `log_msg` to output formatted error
#' messages.
#'
#' @param ... Arguments passed to \code{\link{log_msg}}
#'
#' @examples
#' info("This is an error message")
#'
#' @export
error <- function(...) log_msg(type = "ERROR", ...)

