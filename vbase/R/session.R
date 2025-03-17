#' @title Print information on session and machine
#' 
#' @return Returns output from \code{sessionInfo()}
#' 
#' @description This function prints information on the session and machine.
#' Use capture.output to capture the output and prevent printing to the console.
#'
#' @export
session_info <- function() {    
    cat(paste0("\nUser: ", Sys.getenv("USER"), "\n"))
    cat(paste0("\nPath: ", getwd(), "\n"))
    cat(paste0("\nMachine:\n"))
    print(Sys.info())
    cat(paste0("\nProcess id: ", Sys.getpid(), "\n"))
    cat("\nPackage library paths:\n")
    print(.libPaths())
    cat("\nRepositories:\n")
    print(getOption("repos"))
    cat("\nSession:\n")
	print(sessionInfo())
}
