replace_version <- function(datarelease, options, replacements) {
	stopifnot(length(options) == length(replacements),
		all(!is.na(options)), all(!is.na(replacements)))
	for (i in seq_along(options)) {
		datarelease <- gsub(options[i], replacements[i], datarelease)
	}
	return(datarelease)
}

#' Convert regular V-Dem data release version to V-Party version 
#'
#' Replace original data releases with V-Party releases
#'
#' @param datarelease Vector (character) with data releases information.
#' @param options Vector (character or numeric) of regular data releases in which V-Party data is released as well.
#'
#' @return Character vector of \code{datarelease} length with replacements.
#'
#' @examples
#' \dontrun{
#'VPARTY_RELEASES <- unlist(strsplit("10.11.14", "[.]"))
#'datarelease <- c("10-14.",
#'	"10-14. Available upon request, subject to review and approval",
#'	"10-14.", "14.", "11.")
#'convert_version(datarelease, VPARTY_RELEASES)
#'}
#'
#' @export
convert_version <- function(datarelease, vparty_releases) {
	stopifnot(!all(is.null(vparty_releases)))
	stopifnot(class(datarelease) %in% "character")
	stopifnot(class(vparty_releases) %in% c("character", "numeric"))
	opts <- sort(vparty_releases)
	val1 <- if (opts[1] == 10) 1 else length(10:opts[1])
	repl <- val1:length(opts)

	return(vutils:::replace_version(datarelease, options = opts, replacements = repl))
	}