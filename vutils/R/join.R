#' @export
full_join_vdem <- function(df1, df2, ...) {

    stopifnot(is.data.frame(df1), is.data.frame(df2))
    if (nrow(df1) == 0)
        return(df2)
    if (nrow(df2) == 0)
        return(df1)

    cy_vars <- character(0)
    if ("country_id" %in% names(df1) & "country_id" %in% names(df2))
        cy_vars <- "country_id"
    if ("country_text_id" %in% names(df1) & "country_text_id" %in% names(df2))
        cy_vars <- c(cy_vars, "country_text_id")

    date_vars <- character(0)
    if ("historical_date" %in% names(df1) & "historical_date" %in% names(df2))
        date_vars <- "historical_date"
    if ("year" %in% names(df1) & "year" %in% names(df2))
        date_vars <- c(date_vars, "year")

    if (length(cy_vars) == 0 | length(date_vars) == 0)
        stop("Missing identifier column!")

    info("merging by " %^% paste(c(cy_vars, date_vars), collapse = ", "))

    dplyr::full_join(df1, df2, by = c(cy_vars, date_vars), ...) %>%
        as.data.frame(stringsAsFactors = F)
}


#' @export
full_join_vdem_tree <- function(ll, mc_cores = 10) {

    while (TRUE) {
        n <- length(ll)
        print(n)
        if (n %% 2 == 0) {
            ll <- parallel::mcMap(full_join_vdem, ll[1:(n/2)], ll[((n/2) + 1):n],
                                  mc.cores = mc_cores, mc.preschedule = FALSE)
        } else {
            lll <- ll[2:length(ll)]
            ll <- c(ll[1],
                parallel::mcMap(full_join_vdem, lll[1:(n/2)], lll[((n/2) + 1):n],
                                mc.cores = mc_cores, mc.preschedule = FALSE))
        }
        if (length(ll) == 1)
            break()
    }

    ll[[1]]
}
