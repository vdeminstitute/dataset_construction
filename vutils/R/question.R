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


#' Display country transformations
#'
#' Given a vector of \code{country_ids}s, \code{country_text_ids}, and 
#' \code{country_names} returns all other available transformations.
#'
#' @param countries Character or numeric vector.
#' @param db database connection.
#'
#' @examples
#' \dontrun{
#' country_options(c(4,"USA", "South Korea"))
#' }
#' @export
country_trans <- function(v, db_internal = pg_connect("vdem_data"), full = FALSE) {
    if (!is.vector(v))
        stop("v is not a vector.")
    if (!(is.character(v) | is.numeric(v)))
        stop("Wrong vector type!")

    country <-
        DBI::dbGetQuery(db_internal, "select * from country;") %>% 
        dplyr::rename(country_text_id = text_id, country_name = name) %>%
        vbase::untibble() %>% 
        dplyr::select(country_id, country_text_id, country_name) %>%
        dplyr::filter(!is.na(country_text_id), !grepl("*", country_name, fixed = TRUE))

    if (isTRUE(full))
        return(country)

    if (is.numeric(v))
        return(country[country$country_id %in% v, ])

    # We loop because we want the results to be in the same order
    out <- lapply(v, function(i) {
        if (grepl("^[[:digit:]]+$", i)) {
            if (!i %in% country$country_id)
                 info(i %^% " not found.")
            return(country[country$country_id == as.numeric(i), ])
        }

        if (grepl("^[[:upper:]]+$", i)) {
            if (!i %in% country$country_text_id)
                info(i %^% " not found.")
            return(country[country$country_text_id == i, ])
        }

        if (!i %in% country$country_name)
            info(i %^% " not found.")

        return(country[grepl(i, country$country_name), ])

    }) %>% dplyr::bind_rows(.)

    DBI::dbDisconnect(db_internal)

    return(out)
}