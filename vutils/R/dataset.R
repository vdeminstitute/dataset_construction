#' @export
find_vars <- function(vari, qtable) {

    if (!vari %in% qtable$name)
        stop("The variable is not in the question table")

    if (vari %in% c("e_regionpol_6C", "e_regionpol_7C"))
        return(vari)

	if (vari == "e_uds_median")
		return(c("e_uds_median", "e_uds_mean", "e_uds_pct025", "e_uds_pct975"))

    if (grepl("_\\dC$", vari)) {
        plainvar <- gsub("_\\dC", "", vari)
        return(paste0(plainvar, c("_3C", "_4C", "_5C")))
    }

    df <- qtable[qtable$name == vari, ]

    if (df$to_dichotomize) return(NULL)

    if (is.na(df$additional_versions))
        return(vari)


    # mm
    mm_vars <- c("_codelow", "_codehigh", "_sd",
                 "_osp", "_osp_codelow", "_osp_codehigh", "_osp_sd",
                 "_ord", "_ord_codelow", "_ord_codehigh",
                 "_mean",
                 "_nr")

	# mm + mode
	mm_mode <- c("_codelow", "_codehigh", "_sd",
                 "_osp", "_osp_codelow", "_osp_codehigh", "_osp_sd",
                 "_ord", "_ord_codelow", "_ord_codehigh",
                 "_mean", "_mode",
                 "_nr")

    #other_index
    other_index <- c("_codelow", "_codehigh")

    # index (code_low code_high)
    index_vars <- c("_codelow", "_codehigh", "_sd")

    #percent_vars
    percent_vars <- c("_codelow", "_codehigh", "_sd", "_mean", "_nr")

    # accountability exception
    account_vars <- c("_codelow", "_codehigh", "_osp", "_osp_codelow", "_osp_codehigh")

    # MS no mm
    ms_vars <- c("_nr")

    if (df$additional_versions == "*_nr") {
        varis <- ms_vars
    } else if(df$additional_versions == "*_codelow, *_codehigh, *_sd") {
        varis <- index_vars
    } else if(df$additional_versions == "*_osp, *_ord, *_codelow, *_codehigh, *_sd, *_mean, *_nr") {
        varis <- mm_vars
	} else if (df$additional_versions == "*_osp, *_ord, *_codelow, *_codehigh, *_sd, *_mean, *_mode, *_nr") {
		varis <- mm_mode
    } else if(df$additional_versions == "*_osp, *_codelow, *_codehigh") {
        varis <- account_vars
    } else if(df$additional_versions == "*_codelow, *_codehigh") {
        varis <- other_index
    } else if(df$additional_versions == "*_codelow, *_codehigh, *_sd, *_mean, *_nr") {
        varis <- percent_vars
    } else if (grepl("^e", vari) && !is.na(df$additional_versions)) {
        varis <- gsub("*", "", unlist(strsplit(df[, "additional_versions"], ", ")), fixed = TRUE)
    }

    if (!exists("varis")) {
        out <- vari
    } else {
        out <- c(vari, paste0(vari, varis))
    }

    if (grepl("_\\d+$", vari)) {
        out <- gsub(vari %^% "_nr", gsub("_\\d+$", "", vari) %^% "_nr", out)
    }
    out

}

#' @describeIn trans_helpers Translate question tag names to question labels
#' @export
to_qlabels <- function(v, ttable) {
    # We call this function with the columns of a data.frame, so strip
    # out all the generated codelow/high, sd, mean etc etc.
    basetags <- get_root(v)

    # Unlike to_qids and to_qnames, some of our tag names may not be
    # in our lookup table. Instead of erroring, just return an empty
    # string unless it's historical in which case try the v2 form.
    #basetags <- ifelse(is.hist(basetags) & !basetags %in% ttable$name,
    #                  "v2" %^% substring(basetags, 3),
    #                  basetags)

    out <- character(length(v))
    out[basetags %in% ttable$name] <-
        trans(basetags[basetags %in% ttable$name],
            to = "label", ttable = ttable, by = "name")

    out[is.na(out)] <- ""
    if (any(out == ""))
        info("Missing labels for: " %^% v[out == ""])
    return(out)
}

#' Variable labels
#'
#' Sets either Stata or SPSS variable labels for a data frame by
#' adjusting the attributes of the object.
#'
#' @param x \code{DataFrame}
#' @param labels CharacterVector of labels for each column.
#'
#' @name label
NULL

#' @describeIn label Set SPSS variable labels
#' @export
set_spss_labels <- function(x, labels) {
    if (!inherits(x, "data.frame"))
        stop("SPSS labels can only be set on data frames", call. = F)

    if (length(labels) != ncol(x))
        stop("Length mismatch between labels and columns", call. = F)

    for (i in seq_along(x))
        attr(x[, i], "label") <- labels[i]

    x
}

#' @describeIn label Set Stata variable labels
#' @export
set_stata_labels <- function(x, labels) {
    if (!inherits(x, "data.frame"))
        stop("Stata labels can only be set on data frames", call. = F)

    if (length(labels) != ncol(x))
        stop("Length mismatch between labels and columns", call. = F)

    attr(x, "var.labels") <- labels
    x
}

#' Ordinalize numeric vector
#'
#' Ordinalize an interval scaled numeric vector based on
#' automatic equal-spaced intervals.
#'
#' @param v NumericVector
#' @param categories Desired number of ordinal categories
#' @param max Maximum of \code{v} scale
#' @param min Minimum of \code{v} scale
#' @param vec Bins passed to \code{\link{findInterval}}
#' @param ... Additional arguments passed to \code{ordinalize}
#'
#' @details \code{ordinalize} should not be confused with the
#'     \code{\link{ord}} function. The latter ordinalizes the MM
#'     output using an ordinal scale transformation to map back to the
#'     original ordinal answer categories. The former simply splits up
#'     a numeric vector into equal intervals and assigns the given
#'     number of categories.
#'
#'     Note, ordinalization is is done using left-open intervals. For
#'     example, the lowest category from the default \code{ord_5C}
#'     output is 0 corresponding to \code{N <= .2}, the second is 1
#'     for values \code{.2 < N <= .5}, and etc etc.
#'
#' @section \code{ord_3C}:
#'     The \code{ord_3C} by default operates slightly different from the
#'     other convenience functions. Rather than being a balanced
#'     ordinalization (\emph{i.e.,} equally spaced bins determining
#'     membership in each category), \code{ord_3C} passes in a \code{vec}
#'     of \code{c(0, .25, .5, 1)}. The rationale is that 3 category
#'     versions of indices are meant to represent "Autocratic" (0 -
#'     .25), "Electoral Authoritarian" (.25 - .5), and "Minimally
#'     Democratic" (.5 - 1).
#'
#'
#' @examples
#' (v <- runif(5))
#'
#' ordinalize(v, categories = 5)
#' ord_5C(v)
#'
#' ord_4C(v)
#' ordinalize(v, categories = 4)
#'
#' # Note the difference b/w output
#' ordinalize(v, categories = 3)
#' ord_3C(v)
#'
#' @export
ordinalize <- function(v, categories, max = 1, min = 0,
               vec = seq(min, max, by = max / categories)) {
    if (min(v, na.rm = T) < min | max(v, na.rm = T) > max)
        stop("Out of bounds vector elements", call. = F)

    x <- findInterval(v, vec = vec, left.open = T, all.inside = T)

    (x - 1) / (categories - 1)
}

#' @describeIn ordinalize Ordinalize to 3 categories according to
#'     V-Dem rules. See the section, Details.
#' @export
ord_3C <- function(...) {
    Call <- match.call(expand.dots = T)

    if (is.null(Call[["vec"]])) {
        scale_max <- Call[["max"]] %||% 1
        scale_min <- Call[["min"]] %||% 0

        vec <- c(scale_min,
                .25 * (scale_max - scale_min),
                2 * (.25 * (scale_max - scale_min)),
                scale_max)

        ordinalize(categories = 3, vec = vec, ...)
    } else
        ordinalize(categories = 3, ...)
}

#' @describeIn ordinalize Ordinalize to 4 categories
#' @export
ord_4C <- function(...) ordinalize(categories = 4, ...)

#' @describeIn ordinalize Ordinalize to 5 categories
#' @export
ord_5C <- function(...) ordinalize(categories = 5, ...)


#' @export
sort_multiple_selection <- function(v) {
    original_v <- v
    short_vars <- gsub("_\\d+$", "", v) %>% gsub("_nr$", "", .) 
    vars <- grep_("_\\d+$", v) %>% gsub("_\\d+$", "", .) %>% unique
    
    lapply(vars, function(vv) {
        # vv <- c_vars[1]
        positions <- which(short_vars == vv)
        varvar <- v[positions]
        nums <- gsub("^.*_(\\d+)$", "\\1", varvar)
        nums[grepl("_nr$", nums)] <- NA_character_
        nums <- as.numeric(na.omit(nums)) %>% sort
        outvars <- vv %^% "_" %^% as.character(nums)
        if (vv %^% "_nr" %in% v) {
            outvars <- c(outvars, vv %^% "_nr")
        }
        v[positions] <<- outvars
        return(NULL)
    }) %>% invisible

    stopifnot(
        all(v %in% original_v), 
        all(original_v %in% v),
        length(unique(v)) == length(unique(original_v)))
    return(v)
}