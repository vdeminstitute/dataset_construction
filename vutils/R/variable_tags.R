#' Question tag root
#'
#' Given a vector of question names, \code{get_root} returns the
#' original, non-transformed corresponding tags. Note, ordinalized
#' versions of the HLIs are considered separate question tag, since
#' they have alternative entries in the codebook.
#'
#' @param x CharacterVector of question names. For example, the column
#'     names from the final V-Dem DS.
#'
#' @section Warning: Here be dragons. We will only transform V-Dem
#'     variable tags, meaning that we'll attempt to match \code{v\\d}
#'     or \code{e_v\\d}, while always excluding the direct democracy
#'     variables (v2dd* or v2xdd*) since we never create separate
#'     versions of those.
#'
#' @examples
#' get_root(c("v2clacfree", "v2clacfree_osp", "v2x_freexp_codehigh"))
#'
#' @export
get_root <- function(x) {


    gsub("^(e_v2x|v\\d)(.*?)" %^% paste(c("(_osp$", "_osp_codelow$",
                 "_osp_codehigh$", "_osp_sd$",
                 "_ord$", "_ord_codelow$",
                 "_ord_codehigh$",
                 "_mean$",
                 "_nr$",
                 "_codelow$", "_codehigh$", "_sd$)"), collapse = "|"),
         "\\1\\2", x, perl = FALSE) %>%
    gsub("^(e_v2x.*?)_\\dC$", "\\1_3C", .) %>%
    gsub("gapstart\\d$", "gapstart", .) %>%
    gsub("gapend\\d$", "gapend", .)
}

#' Historical/Contemporary questions
#'
#' Check whether a given question tag is historical (v3) or
#' contemporary (v2).
#'
#' @param x CharacterVector of question tags
#'
#' @examples
#' is.contemp(c("v2clacfree", "v3clacfree"))
#' is.hist(c("v2csreprss", "v3csreprss"))
#'
#' @export
is.contemp <- function(x) {
    substring(x, 1, 2) == "v2"
}

#' @rdname is.contemp
#' @export
is.hist <- function(x) {
    substring(x, 1, 2) == "v3"
}

#' Check if a variable exists for both contemporary and historical
#'
#' \code{is.shared_tag} takes a vector of variable tag names and returns
#' a logical vector indicating which variables exist in both
#' historical and contemporary.
#'
#' @param tags CharacterVector of v2/v3 tag names.
#' @param ttable Translation table listing all contemporary (v2) and
#'     historical (v3) variable tag names (typically our
#'     question_table).
#'
#' @section Warning: \code{is.shared_tag} works only with v2/v3 tag
#'     names. So older variable names lacking the necessary prefix
#'     will always return FALSE, which is actually what we want
#'     because they only exist in the deprecated contemporary surveys.
#'
#' @examples
#' vars <- c("v2clacfree", "v3clacfree", "v3elage", "v2elpdcamp")
#' ttable <- data.frame(name = vars, stringsAsFactors = FALSE)
#'
#' is.shared_tag(vars, ttable)
#'
#' @return LogicalVector
#'
#'@export
is.shared_tag <- function(tags, ttable) {
    if (!"name" %in% colnames(ttable))
        stop("Missing name column in ttable")

    if (any(!tags %in% ttable$name))
        stop("Missing values in ttable from " %^% deparse(substitute(v)))

    roots <- substring(tags, 3)
    ifelse(("v3" %^% roots) %in% ttable$name & ("v2" %^% roots) %in% ttable$name, T, F)
}

#' Get survey from question tag
#'
#' Given a vector of question tags, \code{get_survey} returns the
#' two letter survey abbreviation for A*, A, B, and C-variables
#'
#' @param x CharacterVector of question tags.
#'
#' @examples
#' get_survey(c("v2clacfree", "v2dlcountr"))
#'
#' @export
get_survey <- function(x) {

    if (any(!grepl("^v\\d[a-z]+$", x)))
        stop("Invalid input, expected an A*, A, B, C tag name.", call. = F)

    substr(x, 3, 4)
}