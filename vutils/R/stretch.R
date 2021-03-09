#' Expand based on row.names
#'
#' \code{stretch} expands a matrix row-wise by a given character
#' vector of country-dates. For example, it can be used to expand a
#' country-date reduced matrix to the full time series.
#'
#' @param x Matrix to expand.
#' @param by CharacterVector of country-dates to expand by.
#' @param gaps Logical, whether to preserve gaps when expanding.
#' @param preserve.na Logical, whether to preserve NA from the original
#'     matrix, \code{x}.
#' @param interpolate Logical, whether to first interpolate each year
#'     before filling in missing values in the expanded matrix.
#'
#' @details Given a matrix, \code{x}, with country-date rownames and a
#'     character vector of country-dates that should be added
#'     (\emph{i.e.,} expanded to), \code{stretch} will output a matrix
#'     with \code{ncol(x)} columns and \code{length(union(by,
#'     rownames(x)))} rows. The rows shared between the output matrix
#'     and \code{x} will be filled in with the values from \code{x}
#'     before calling \code{\link{locf}}.
#'
#'     Missingness will be filled in by \code{country_text_id}, sorted
#'     by \code{historical_date}. If \code{gaps} is \code{TRUE}, gaps
#'     will be preserved such that if there is more than a 365 day
#'     difference between two observations, the last observation will
#'     not be carried forward.
#'
#'     When \code{preserve.na} is \code{TRUE}, \code{NA} in the
#'     original matrix, \code{x}, will be preserved and propagated in
#'     the output matrix.
#'
#'     If \code{interpolate} is \code{TRUE}, before calling
#'     \code{\link{locf}}, values in the output matrix will be first
#'     interpolated per country-year using the
#'     \code{\link{interpolate}} --- specifically, this means that if
#'     there's a single non-missing observation on "12-31", the year
#'     will first be backfilled.
#'
#' @examples
#' m <- matrix(1:2, 2, 1, dimnames = list(c("AFG 1790-12-31", "AFG 1791-12-31"),
#'                                        NULL))
#'
#' full_names <- c("AFG 1790-12-31", "AFG 1791-05-04",
#'                 "AFG 1791-12-31", "AFG 1795-12-31")
#'
#' stretch(m, full_names)
#'
#' # Expand with interpolation
#' stretch(m, full_names, interpolate = TRUE)
#'
#' # Ignore gaps
#' stretch(m, full_names, gaps = FALSE)
#'
#' @family fill functions
#' @export
stretch <- function(x, by, gaps = TRUE, rule_366 = TRUE, preserve.na = TRUE, interpolate = FALSE,
                    utable = NULL, party = FALSE, elecreg_cy = NULL) {UseMethod("stretch")}

#' @export
stretch.matrix <- function(x, by, gaps = TRUE, rule_366 = TRUE, preserve.na = TRUE,
                           interpolate = FALSE, utable = NULL, party = FALSE, 
                           elecreg_cy = NULL) {
    assert_str(by, party)
    assert_str(rownames(x), party)

    if (setequal(rownames(x), by))
        return(x[order(rownames(x)),, drop = F])

    full_names <- union(by, rownames(x))
    full.ma <- matrix(NA, length(full_names), ncol(x), dimnames = list(full_names, colnames(x)))

    full.ma[match(rownames(x), full_names), ] <- x
    full.ma <- full.ma[sort(rownames(full.ma)),, drop = F]

    # If TRUE we preserve NA from `x` and carry it forward in the
    # full.ma matrix since we assume that it represents missingness
    # that we want retained. Unfortunately, we're running up against
    # the R type system here so we have no meaningful way to
    # distinguish NAs from expanding and NAs originally in `x`. As an
    # ugly hack, replace the NAs from `x` with Inf and convert back in
    # the end.
    if (isTRUE(preserve.na)) {
        b <- rowSums(is.na(x)) == ncol(x)

        if (any(b))
            full.ma[rownames(full.ma) %in% rownames(x)[b], ] <- Inf
    }

    factors <- rownames(full.ma) %>% sub(" \\d{4}-\\d{2}-\\d{2}$", "", x = .)
    dates <- rownames(full.ma) %>% get_date(party)

    if (isTRUE(gaps)) {

        if (rule_366) {
            gaps <- split(dates, factors) %>%
                lapply(create_idx) %>%
                unlist
            if (any(is.na(gaps)))
                stop("Unable to set gaps", call. = F)
            factors <- factors %^% gaps
        } else {
            if (is.null(utable))
                stop("utable is missing!")

            join_cols <- c("country_text_id", "year")

            gaps <-
                data.frame(country_text_id = rownames(full.ma) %>% get_text_id(party),
                           historical_date = rownames(full.ma) %>% get_date(party),
                           stringsAsFactors = F)

            if (party) {
                join_cols <- c("country_text_id", "party_id", "historical_date")
                gaps[["party_id"]] <- rownames(full.ma) %>% get_party_id
            }

            if (is.null(elecreg_cy)) {
                gaps <- gaps %>%
                    dplyr::mutate(year = to_year(historical_date)) %>%
                    dplyr::left_join(utable, by = join_cols) %>%
                    dplyr::pull(gap_idx)
            } else {
                # We do not want to interpolate election specific mm variables
                # into regimes where v2x_elecreg is 0.
                utable_merged <- utable_join_elecreg_index(utable, elecreg_cy)
                gaps <- gaps %>%
                    dplyr::mutate(year = to_year(historical_date)) %>%
                    dplyr::left_join(utable_merged, by = join_cols) %>%
                    dplyr::pull(combined_idx)
            }
            

            if (any(is.na(gaps)))
                stop("Unable to set gaps", call. = F)

            factors <- gaps
        }



    }

    if (isTRUE(interpolate)) {
        years <- to_year(dates)

        # We really should think about simplifying this
        full.ma <- by_split(full.ma, factors %^% years,
                           methods::getFunction("interpolate"))
        full.ma <- full.ma[order(rownames(full.ma)),, drop = F]
    }

    out <- by_split(full.ma, factors, locf)
    out[is.infinite(out)] <- NA
    out <- out[order(rownames(out)),, drop = F]

    return(out)
}


#' @export
mm_stretch_z_sample <- function(ll, utable) {
    stretch(load_matrix(ll[[1]]$post.sample$z),
            ll[[1]]$country_dates,
            rule_366 = TRUE,
            interpolate = TRUE,
            utable = utable)
}

#' @export
binary_stretch_z_sample <- function(ll, utable) {
    stretch(load_matrix(ll[[1]]$post.sample$z),
            ll[[1]]$country_dates,
            rule_366 = TRUE,
            interpolate = TRUE,
            utable = utable)
}

#' @export
bfa_stretch_z_sample <- function(ll, utable) {
    stretch(load_matrix(ll[[1]]$thin_post), 
            ll[[1]]$country_dates, 
            rule_366 = TRUE, 
            interpolate = TRUE, 
            utable = utable)
}

# Expects a named list of matrices and returns a named list of matrices

#' @export
stretch_combined <- function(ll, utable) {
    stopifnot(lapply(ll, is.matrix) %>% unlist)
    nn <- names(ll)
    combined_names <- lapply(ll, rownames) %>% unlist %>% unique %>% sort
	info(sprintf("Found %d combined country-dates", length(combined_names)))
	out <- Map(function(m, nam) {
        info("Stretching " %^% nam)
        stretch(m, combined_names, interpolate = T, utable = utable)
    }, m = ll, nam = nn)
    
    stopifnot({
        first_rownames <- rownames(out[[1]])
        unlist(lapply(out, function(v) {
            all_identical(first_rownames, rownames(v))}))
    })
    
    return(out)
}

#' @export
calc_elecreg_index <- function(v) {
    change <- diff(v)
    change[change == -1] <- 1
    cumsum(c(1, change))
}

#' @export
utable_join_elecreg_index <- function(utable, elecreg_cy) {
    if ("historical_date" %in% names(elecreg_cy))
        elecreg_cy %<>% dplyr::select(-historical_date)
    dplyr::left_join(utable, elecreg_cy, by = c("country_id", "year")) %>%
        dplyr::group_by(gap_idx) %>%
        dplyr::mutate(elecreg_idx = calc_elecreg_index(v2x_elecreg)) %>%
        dplyr::group_by(gap_idx, elecreg_idx) %>%
        dplyr::mutate(combined_idx = dplyr::cur_group_id())
}

#' @export
front_filling_els_variables <- function(m, utable, elecreg_cy, elecreg_cd, country, party = FALSE) {
    join_cols <- c("country_text_id", "year")
    elecreg_cd %<>% dplyr::left_join(dplyr::select(country, country_id, country_text_id), by = "country_id")
    df <- data.frame(country_text_id = rownames(m) %>% get_text_id(party),
                     historical_date = rownames(m) %>% get_date(party)) %>%
        dplyr::left_join(dplyr::select(elecreg_cd, country_text_id, historical_date, 
                         v2x_elecreg_cd = v2x_elecreg), 
                  by = c("country_text_id", "historical_date"))
    utable_merged <- utable_join_elecreg_index(utable, elecreg_cy) %>%
        dplyr::select(-historical_date)
    
    df %<>% 
        dplyr::mutate(year = to_year(historical_date)) %>%
        dplyr::left_join(utable_merged, by = join_cols)
    nn <- colnames(m)
    rr <- rownames(m)
    m_df <- as.data.frame(m) %>%
        dplyr::bind_cols(dplyr::select(df, country_text_id, historical_date, year, gap_idx, 
            combined_idx, v2x_elecreg, v2x_elecreg_cd))

    fu <- function(subdf) {
        # print(distinct(subdf, country_text_id, historical_date))
        bool <- is.na(subdf[, 1])
        # If first value is not NA return subdf
        if (!bool[1])
            return(subdf)
        first_non_na <- which(!bool)
        if (length(first_non_na) == 0)
            return(subdf)
        # First matching row with data
        first_non_na <- first_non_na[1]
        # Is v2x_elecreg 1 ?
        # We don't want to front fill if there is a case with elecreg 0
        # (e.g. Germany 1949)
        if (all(subdf$v2x_elecreg[1:(first_non_na - 1)] == 1) & 
                ((!any(subdf$v2x_elecreg_cd[1:(first_non_na - 1)] == 0)) %||% FALSE)) {
            subdf[1:(first_non_na - 1), ] <- 
                do.call("rbind", replicate(first_non_na - 1, 
                                           subdf[first_non_na, ], 
                                           simplify = FALSE))
            info(nn[1] %^% ": Front-filled " %^% rownames(subdf[1:(first_non_na - 1), ]) %^% " from " %^% rownames(subdf[first_non_na, ]))
        }
        return(subdf)
    }

    m_df_done <- split.data.frame(m_df, df$gap_idx) %>% 
        lapply(., fu) %>%
            dplyr::bind_rows(.)
    rr <- paste0(m_df_done$country_text_id, " ", m_df_done$historical_date)
    m_df_done %<>%
            dplyr::select(-country_text_id, -historical_date, -year, -gap_idx, 
                -combined_idx, -v2x_elecreg, -v2x_elecreg_cd) %>%
            data.matrix(.)
    # rownames(m_df_done) <- rr
    colnames(m_df_done) <- nn
    m_df_done <- m_df_done[rownames(m), ]

    # Check that non-NA observations are identical!
    bool <- !is.na(m[, 1])
    stopifnot(identical(m[bool, ], m_df_done[bool, ]))
    
    return(m_df_done)
}