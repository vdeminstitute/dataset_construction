#' @title Check if a path is mounted
#' @description This function checks whether a path is mounted in the file system.
#' @param PATH Character string with the path to be checked. Defaults to the "MOUNT_PATH" environment variable.
#' @return A logical value indicating whether the path is mounted or not.
#' @export
is_path_mounted <- function(PATH = Sys.getenv("MOUNT_PATH")) {
	mm <- suppressWarnings(
			system("mountpoint " %^% PATH, 
				intern = TRUE, 
				ignore.stdout = FALSE, 
				ignore.stderr = TRUE))
	return(isTRUE(grepl("is a mountpoint", mm)))
}