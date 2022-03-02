#!/usr/bin/env Rscript

# ==========================================================================
# Given the interpolated output from `prep_transform.R`, collapse
# observations to changes by:
#   Step 1. Combine confidences and ratings into one matrix
#        2. Subset combined matrix by boolean vector indicating all
#           changes (including missing obs)
#        3. Reduce again by collapsing rows where ratings/conf don't
#           change values (and fill missing obs)
#               - Backfill ratings/confs unless month level change
#        4. Split final reduced df back into ratings and confidences
#
# ==========================================================================

options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
suppressMessages(library(data.table))
set_env(MODULE_NAME = "reduce")

#
# Function
# --------------------------------------------------------------------------

sort_matrix_by_rownames <- function(m, decreasing = FALSE) {
    m[order(rownames(m), decreasing = decreasing), , drop = FALSE]
}

identify_reduction_groups <- function(m) {

    # Stop if matrix is not sorted correctly!
    stopifnot(identical(rownames(m), sort(rownames(m))))

    df <- data.frame(
        country_text_id = get_text_id(rownames(m)),
        historical_date = get_date(rownames(m)), 
            stringsAsFactors = FALSE)
    df %<>% 
        dplyr::group_by(country_text_id) %>% 
        dplyr::mutate(gaps = cumsum(c(FALSE, diff(historical_date) > 366))) %>% 
        dplyr::ungroup(.)

    idx <- 
        df %>%
        dplyr::group_by(country_text_id, gaps) %>%
        dplyr::group_indices(.)

    compare_rows <- function(v1, v2) {
        all(na.omit(v1 == v2))
    }

    fill_row <- function(v1, v2) {
        v1[is.na(v1)] <- v2[is.na(v1)]
        v1
    }

    group_ids <- 
        split.data.frame(m, idx) %>%
        lapply(function(subM) {
            groupV <- numeric(nrow(subM))
            vtrack <- subM[1, ]
            groupV[1] <- TRUE
            if (nrow(subM) == 1)
            return(TRUE)

            for (i in 2:nrow(subM)) {
                # i <- 1
                vcurrent <- subM[i, ]
                if(compare_rows(vtrack, vcurrent)) {
                    vtrack <- fill_row(vtrack, vcurrent)
                    groupV[i] <- FALSE
                } else {
                    vtrack <- vcurrent
                    groupV[i] <- TRUE
                }                         
            }
            groupV
        }) %>%
        Reduce(c, .) %>% 
        cumsum
    group_ids
}


interpolate_within_groups <- function(m, group_id) {
    stopifnot(identical(rownames(m), sort(rownames(m))))
    split.data.frame(m, group_id) %>%
        lapply(., function(mm) {
            mm <- mm[order(rownames(mm), decreasing = TRUE), , drop = FALSE]
            mm <- vutils::locf(mm)
            mm <- mm[order(rownames(mm), decreasing = FALSE), , drop = FALSE]
            mm <- vutils::locf(mm)
            mm
    }) %>% do.call(rbind, .)
}


reduce_groups <- function(m, group_id) {
    stopifnot(identical(rownames(m), sort(rownames(m))))
    group_id_to_boolean <- function(v) {
        out <- v != dplyr::lag(v)
        out[1] <- TRUE
        out
    }
 
    m[group_id_to_boolean(group_id), , drop = FALSE]
}


date_to_weights_within_year <- function(v) {
    if (length(v) == 1)
        return(1)
    v <- sort(v, decreasing = FALSE)
    diffdates <- c(diff(v), 1)
    as.numeric(diffdates)/sum(as.numeric(diffdates))
}



date_to_weights_across_years <- function(v) {
    stopifnot(class(v) == "Date")
    if (length(v) == 1)
        return(1)
    nyears <- lubridate::year(v) %>% unique %>% length
    out <- split(v, lubridate::year(v)) %>%
    lapply(., date_to_weights_within_year) %>% unlist %>% {./nyears}
    names(out) <- NULL
    out
}


reduce_confidences <- function(m, group_id) {
    stopifnot(identical(rownames(m), sort(rownames(m))))
    outi <-
        split.data.frame(m, group_id) %>%
        lapply(., function(mm) {


            mm %<>% 
                sort_matrix_by_rownames(decreasing = TRUE) %>% locf %>%
                sort_matrix_by_rownames(decreasing = FALSE) %>% locf
            dates <- gsub("^(.*?)(\\d{4}-\\d{2}-\\d{2})$", "\\2", rownames(mm)) %>% as.Date
            weights <- date_to_weights_across_years(dates)
            fu <- function(v, weights, ...) {weighted.mean(v, weights, ...)}
            outM <- matrix(apply(mm, 2, function(v) fu(v, weights = weights)), 
                nrow = 1, 
                dimnames = list(rownames(mm)[1], colnames(mm)))
            outM

    }) %>% do.call(rbind, .)
    outi %<>% 
        sort_matrix_by_rownames %>%
        round(., 3)
    is.na(outi) <- is.na(outi)

    outi
}

rename_cols_to_work <- function(input.data) {
    stopifnot(is.list(input.data),
        all(c("wdata", "conf_mat") %in% names(input.data)))

    colnames(input.data$wdata) <- "code_" %^% colnames(input.data$wdata)
    colnames(input.data$conf_mat) <- "conf_" %^% colnames(input.data$conf_mat)
    return(input.data)
}

define_reduce_groups <- function(wdata) {
    wdata %>% 
        sort_matrix_by_rownames %>%
        identify_reduction_groups %>%
        return(.)
}

reduce_wdata <- function(wdata, group_index) {
    interpolated_wdata <- wdata %>% 
        sort_matrix_by_rownames %>%
        interpolate_within_groups(group_index)

    reduced_data <- reduce_groups(interpolated_wdata, group_index)
    colnames(reduced_data) <- sub("code_", "", colnames(reduced_data))
    return(reduced_data)
}

reduce_conf <- function(conf_mat, group_index) {
    reduced_conf <- conf_mat %>%
        sort_matrix_by_rownames %>%
        reduce_confidences(group_index)
    colnames(reduced_conf) <- sub("conf_", "", colnames(reduced_conf))
    return(reduced_conf)
}

corr_order <- function(wdata) {
    idx <- order(rownames(wdata))
}

checks <- function(input.data) {
    stopifnot(is.list(input.data),
        all(c("wdata", "conf_mat") %in% names(input.data)))
    stopifnot(!is.null(rownames(input.data$wdata)), !is.null(rownames(input.data$conf_mat)))
    stopifnot(identical(dim(input.data$wdata), dim(input.data$conf_mat)))
    stopifnot(identical(is.na(input.data$wdata), is.na(input.data$conf_mat)))
    stopifnot(identical(rownames(input.data$wdata), rownames(input.data$conf_mat)))
    stopifnot(!any(is.nan(input.data$wdata)))
    stopifnot(!any(is.nan(input.data$conf_mat)))
}

main <- function(input.data) {
    input.data <- rename_cols_to_work(input.data)
    group_index <- define_reduce_groups(input.data$wdata)
    wdata <- reduce_wdata(input.data$wdata, group_index)
    conf <- reduce_conf(input.data$conf_mat, group_index)
    input.data$wdata <- wdata[corr_order(wdata), ]
    input.data$conf_mat <- conf[corr_order(wdata), ]
    checks(input.data)
    return(input.data)
}


#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    input.data <- find_dep_files(TASK_ID, DB)[[VARNAME]][[VARNAME]]

    info("Reducing: " %^% VARNAME)
    # Run
    collectedInputs <- named_list(input.data)
    out <- setNames(list(do.call(main, collectedInputs)), VARNAME)
    stopifnot(!is.null(rownames(out[[VARNAME]]$wdata))) 
    write_file(out, OUTFILE, dir_create = TRUE)
    info("Reduction done for " %^% VARNAME)
} else {
    # Tests
    testthat::test_file("~/proj/mm-prep/tests/mm_prep/test_reduce.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)
