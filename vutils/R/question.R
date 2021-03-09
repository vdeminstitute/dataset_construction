#' Normalize historical \code{question_id}s
#'
#' Given a vector of \code{question_id}s, \code{normalize_qids}
#' replaces the historical \code{question_id}s with the analogous
#' contemporary \code{question_id}s where there's a match based on
#' \code{ttable}.
#'
#' @param ids NumericVector of \code{question_id}s.
#' @param ttable \code{question_id} translation table (\emph{e.g.},
#'     the question table).
#'
#' @details \code{normalize_qids} is fairly inflexible and is meant to
#'     work directly with the question table. Thus, \code{ttable}
#'     requires two columns: \code{name} and \code{question_id}. We
#'     determine whether an historical \code{question_id} has a
#'     matching contemporary \code{question_id} by checking the root
#'     tag --- the portion of the tag after removing the \code{v\\d}
#'     suffix.
#'
#' @section Warning: We are currently not checking whether matching
#'     historical and contemporary \code{question_id}s have the same
#'     \code{K} (\emph{i.e.}, number of answer categories). This is
#'     mostly because our information on \code{K} is fairly
#'     incomplete; plus, there are a number of variables such as
#'     \code{v3lgbicam} which diverge from contemporary and yet we
#'     still want to merge them together.
#'
#'     This is something that can be improved in the future.
#'
#' @examples
#' ttable <- data.frame(question_id = 1:3,
#'                      name = c("v2clacfree", "v3clacfree", "v3strenadm"),
#'                      stringsAsFactors = FALSE)
#'
#' normalize_qids(1:3, ttable)
#'
#' @export
normalize_qids <- function(ids, ttable) {
    if (is.null(ttable))
        stop("Missing translation table")

    tags <- trans(ids, "name", ttable, "question_id")
    roots <- sub("v3", "v2", tags)
    new_ids <- trans(roots, "question_id", ttable, "name")

    # ids[roots %in% ttable$name & roots %in% tags] <-
    #    trans(roots[roots %in% ttable$name & roots %in% tags], "question_id", ttable, "name")

    # assertion exception for v2lgbicam / v3lgbicam
    #ids_test <- ids
    #ids_test<- ifelse(ids_test == 1379,
    #                      595,
    #                      ids_test)
    #stopifnot(identical(trans(ids_test, "k", ttable, "question_id"),
    #                    trans(new_ids, "k", ttable, "question_id")))

    new_ids
}


