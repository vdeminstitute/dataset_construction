#' @export
clean_by_utable <- function(x, utable) {UseMethod("clean_by_utable")}

#' @export
clean_by_utable.default <- function(x, utable) {
    stop("No defined method for this object!")
}

#' @export
clean_by_utable.data.frame <- function(x, utable) {
    stopifnot(c("year", "country_id") %in% names(x))
    dplyr::inner_join(x, select(utable, country_id, year),
                      by = c("country_id", "year")) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

#' @export
clean_by_utable.matrix <- function(x, utable) {
    df <- data.frame(
        country_text_id = substr(rownames(x), 1, 3),
        historical_date = substr(rownames(x), 5, 8) %^% "-12-31",
        stringsAsFactors = F) %>%
        dplyr::left_join(
            dplyr::select(utable, country_text_id,
                          historical_date, project),
            by = c("country_text_id", "historical_date"))
    bool <- !is.na(df$project)
    x[bool, ]
}

#' @export
check_utable <- function(df, utable) {
    stopifnot(c("year", "country_id") %in% names(df))
    dplyr::anti_join(df, select(utable, country_id, year),
                     by = c("country_id", "year")) %>%
        nrow %>% . == 0
}
