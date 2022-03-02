#' @export
clean_by_utable <- function(x, utable, party = FALSE) {UseMethod("clean_by_utable")}

#' @export
clean_by_utable.default <- function(x, utable, party = FALSE) {
    stop("No defined method for this object!")
}

#' @export
clean_by_utable.data.frame <- function(x, utable, party = FALSE) {
    stopifnot(c("year", "country_id") %in% names(x))
    stopifnot(!any(is.na(x$year)))
    stopifnot(!any(is.na(x$country_id)))
    dplyr::inner_join(x, utable[, "country_id", "year"],
                      by = c("country_id", "year")) %>%
    # This may remove tibble grouping, be careful!
    as.data.frame(stringsAsFactors = FALSE)
}

#' @export
clean_by_utable.matrix <- function(x, utable, party = FALSE) {
    assert_str(x, party)
    df <- data.frame(
        country_text_id = get_text_id(x, party),
        historical_date = get_date(x, party) %>%
        to_year %>%
        paste0("-12-31"),
        stringsAsFactors = FALSE) %>%
        dplyr::left_join(
            dplyr::select(utable, country_text_id,
                          historical_date, project),
            by = c("country_text_id", "historical_date"))
    bool <- !is.na(df$project)
    x[bool, ]
}


#' @export
remove_observations <- function(df, remove_bool) {
    stopifnot(!anyNA(remove_bool))
    dirt <- df[remove_bool, ]
    # print_function(dirt)
    list(clean = df[!remove_bool, ], dirty = dirt)
}

#' @export
remove_nonc_duplicates <- function(df, col_names) {
    stopifnot(all(col_names %in% names(df)) | "id" %in% names(df))
    if (!all(df$class %in% c("A*", "A", "A,C", "B", "D")))
        return(list(clean = df, dirty = data.frame()))
    keep_nonc <- df %>%
        dplyr::filter(class %in% c("A*", "A", "A,C", "B", "D")) %>%
        dplyr::arrange(!!!syms(col_names), dplyr::desc(id)) %>%
        dplyr::distinct(!!!syms(col_names), .keep_all = T) %$% id
    remove_nonc <-
        df %>%
        dplyr::filter(class %in% c("A*", "A", "A,C", "B", "D")) %>%
        dplyr::filter(!id %in% keep_nonc) %>%
        pull(id)
    remove_observations(df, df$id %in% remove_nonc)
}

#' @export
remove_c_duplicates <- function(df, col_names) {
    stopifnot(all(col_names %in% names(df)) | "id" %in% names(df))
    if (!all(df$class %in% c("C")))
        return(list(clean = df, dirty = data.frame()))
    stopifnot(length(unique(df$id)) == nrow(df))
    keep_c <-
        df %>%
        dplyr::arrange(!!!syms(col_names), dplyr::desc(id)) %>%
        dplyr::distinct(!!!syms(col_names),
                 .keep_all = T) %>% 
        pull(id)
    remove_c <-
        df %>%
        dplyr::filter(!id %in% keep_c) %>%
        pull(id)
    out <- remove_observations(df, df$id %in% remove_c)
    stopifnot(no_duplicates(out$clean, col_names))
    return(out)
}

#' @export
remove_ms_duplicates <- function(df) {
    keep_ms <-
        df %>%
        dplyr::filter(question_type == "S") %>%
        dplyr::arrange(dplyr::desc(id)) %>%
        dplyr::distinct(question_id, country_id, coder_id, historical_date, code,
                 .keep_all = T) %$% id
    remove_ms <-
        df %>%
        dplyr::filter(question_type == "S") %>%
        dplyr::filter(!id %in% keep_ms) %$% id
    remove_observations(df, df$id %in% remove_ms)
}

#' @export
print_by_name <- function(df) {
        df %>%
        dplyr::group_by(name, class) %>%
        dplyr::summarize(n = n()) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        as.data.frame(stringsAsFactors = F) %>%
        print
}

#' @export
print_by_country <- function(df) {
        df %>%
        dplyr::group_by(country_name) %>%
        dplyr::summarize(n = dplyr::n()) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        as.data.frame(stringsAsFactors = F) %>%
        print
}

#' @export
print_by_country_year <- function(df) {
        df %>%
        dplyr::group_by(country_name, year) %>%
        dplyr::summarize(n = dplyr::n()) %>%
        dplyr::arrange(country_name, year) %>%
        as.data.frame(stringsAsFactors = F) %T>%
        print
}



