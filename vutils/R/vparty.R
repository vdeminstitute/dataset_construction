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
#'VPARTY_RELEASES <- unlist(strsplit("10.12.14", "[.]"))
#'datarelease <- c("10-14.",
#'	"10-14. Available upon request, subject to review and approval",
#'	"10-14.", "14.", "11.", "9-12", "9-10.")
#'convert_version(datarelease, VPARTY_RELEASES)
#'}
#'
#' @export
convert_version <- function(datarelease, vparty_releases) {
	stopifnot(!all(is.null(vparty_releases)))
	stopifnot(class(datarelease) %in% "character")
	stopifnot(class(vparty_releases) %in% c("character", "numeric"))
	opts <- sort(as.numeric(vparty_releases))
	val1 <- 1
	repl <- val1:length(unique(opts))

	datarelease_repl <- vutils:::replace_version(datarelease, options = opts, replacements = repl)

	opts_over <- gsub("(^\\d{1,2}[-]?\\d{1,2}?)(\\. .*)", "\\1", datarelease) %>%
		gsub("\\.$", "", x = .) %>%
	unique() %>%
	strsplit("-") %>%
	lapply(function(x) x[[1]]) %>%
	unlist() %>%
	as.numeric() %>%
	unique() %>%
	.[. < as.numeric(opts[1])] %>%
	paste0("-")

	repl_over <- paste0(rep(val1, length(opts_over)), "-")

	datarelease_out <- vutils:::replace_version(datarelease_repl, options = opts_over, replacements = repl_over)

	if (any(grepl("^1-1.", datarelease_out))) {
		datarelease_out <- gsub("^1-1.", "1.", datarelease_out)
	}

	return(datarelease_out)
	}