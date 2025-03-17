#!/usr/bin/env Rscript

# ==========================================================================
# The purpose of this script is to produce reduced observations for the
# measurement model. 
#
# A reduction *period* are sequences of chronological across only consecutive
# years ratings where no expert provided any new information. This definition
# applies for a given country, and for all experts that provided ratings for
# that particular country.
#
# Within a reduction period, we first interpolate ratings backwards in time
# and then forwards in time. In the end, we use the unique rating for such a
# period to define reduced *observations*.
# 
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Function
# --------------------------------------------------------------------------

# Sort by rownames: the matrices have ID as rownames
sort_matrix_by_rownames <- function(m, decreasing = FALSE) {
    stopifnot(is.matrix(m))
    stopifnot(!any(rownames(m) == ""))

    m[order(rownames(m), decreasing = decreasing), , drop = FALSE]
}

# Rename columns 
rename_cols_to_work <- function(input.data) {
    stopifnot(is.list(input.data), all(c("wdata", "conf_mat") %in% names(input.data)))
    stopifnot(!any(colnames(input.data$wdata) == ""))
    stopifnot(!any(colnames(input.data$conf_mat) == ""))

    # Append code_ and conf_ to colnames of wdata and conf_mat
    colnames(input.data$wdata) <- sprintf("code_%s", colnames(input.data$wdata))
    colnames(input.data$conf_mat) <- sprintf("conf_%s", colnames(input.data$conf_mat))

    return(input.data)
}


compare_rows <- function(v1, v2) {
    stopifnot(!is.null(v1), !is.null(v2))
    stopifnot(length(v1) == length(v2))
    # If both v1 and v2 are NA, na.omit return 1
    # all(1) returns TRUE.
    all(na.omit(v1 == v2))
}

fill_row <- function(v1, v2) {
    # from v2, copy over value where v1 is missing
    # and replace v1 value.
    # if v2 is not missing at this position, value
    # in v1 will be updated.
    v1[is.na(v1)] <- v2[is.na(v1)]
    v1
}

# This function identifies the "coding regimes" groups to reduce to single observations 
identify_reduction_groups <- function(m) {

    # Stop if matrix is not sorted correctly!
    stopifnot(identical(rownames(m), sort(rownames(m))))

    # Create df to generate group identifiers from
    df <- data.frame(
        country_text_id = get_text_id(rownames(m)),
        historical_date = get_date(rownames(m)), 
            stringsAsFactors = FALSE)
    # Add gaps as cumulative sum of booleans: starts at 0, increments by 1 
    # whenever the difference in days between t and t-1 is > 366 days.
    # idx will then start at 1 following group_indices()
    idx <- df |>   
        dplyr::group_by(country_text_id) |>  
        dplyr::mutate(gaps = cumsum(c(FALSE, 
			(diff(historical_date) > 366) |
			diff(to_year(historical_date)) > 1))) |> 
        dplyr::ungroup() |>  
        dplyr::group_by(country_text_id, gaps) |> 
        dplyr::group_indices()

	stopifnot(identical(idx, sort(idx)))
    group_ids <- 
        split.data.frame(m, idx) |> 
        lapply(X=_, function(subM) {
            groupV <- numeric(nrow(subM)) # create dbl with 0 of length |subM|
            vtrack <- subM[1, ] # first row of the split
            groupV[1] <- TRUE # first row of group is always part of the group
            if (nrow(subM) == 1) # return if there only is one observation
            return(TRUE)

            for (i in 2:nrow(subM)) {
                vcurrent <- subM[i, ]
                if(compare_rows(vtrack, vcurrent)) {
                    vtrack <- fill_row(vtrack, vcurrent) # carry "no new info" forwards
                    groupV[i] <- FALSE # set groupV to FALSE for cumsum
                } else {
                    vtrack <- vcurrent # update vtrack as vcurrent
                    groupV[i] <- TRUE # update groupV for cumsum
                }                         
            }
            return(groupV)
        }) |> 
        Reduce(f = c, x = _) |>  # reduce into one vector
        cumsum() # define stable regimes

	names(group_ids) <- rownames(m)
	stopifnot(identical(sort(group_ids), group_ids))
    return(group_ids)
}

# within every reduction group, scores are filled backwards and then forwards.
# we can do this, because for the measurement model we only use the unique scores
# from a given reduction group.
interpolate_within_groups <- function(m, group_id) {
    stopifnot(identical(rownames(m), sort(rownames(m))))
    split.data.frame(m, group_id) |> 
        lapply(X=_, function(mm) {
            # sort descending and fill forward (backwards in chronological time)
            mm <- mm[order(rownames(mm), decreasing = TRUE), , drop = FALSE]
            mm <- vutils::locf(mm)
            # sort ascending and fill fowards
            mm <- mm[order(rownames(mm), decreasing = FALSE), , drop = FALSE]
            mm <- vutils::locf(mm)
            return(mm)
        }) |> do.call(what = rbind, args = _)
}

# Helper
group_id_to_boolean <- function(v) {
    stopifnot(!anyNA(v))
    # Return FALSE if lag equals current equals (no change in id)
    # Return TRUE for new regime start
    out <- v != vbase::lagVector(v)
    # Set first to TRUE because first is always start
    out[1] <- TRUE
    out
}

# reduce periods, that can have +1 rows, to one row per reduction period
reduce_groups <- function(m, group_id) {

    stopifnot(identical(rownames(m), sort(rownames(m))))
	stopifnot(identical(rownames(m), names(group_id)))

    # Define first row of all reduction groups
	index <- group_id_to_boolean(group_id)
    stopifnot(length(index) == nrow(m))

    # Return only the first row of every regime.
    # Interpolated data is now reduced.
    m_out <- m[index, , drop = FALSE]
    info(sprintf("From %d to %d observations after reduce.",
        nrow(m), nrow(m_out)))

    return(m_out)
}

# calculates the difference in days on historical_date
date_to_weights_within_year <- function(v) {
    if (length(v) == 1) {
        return(1)
    }
    
    # Sort ascending
    v <- sort(v, decreasing = FALSE)
    # Compute difference in days and append 1 to for the last date
    diffdates <- c(diff(v), 1)
    # Output ratio of individual difference to sum of differences as numeric
    out <- as.numeric(diffdates)/sum(as.numeric(diffdates))
    stopifnot(length(v) == length(out))
    return(out)
}

date_to_weights_across_years <- function(v) {

    stopifnot("Date" %in% class(v))
    if (length(v) == 1)
        return(1)
    nyears <- length(unique(lubridate::year(v))) 
    out <-
        split(v, lubridate::year(v)) |> 
        lapply(X=_, date_to_weights_within_year) |> 
        unlist()
    out <- out / nyears

    names(out) <- NULL
    stopifnot(length(v) == length(out))
    return(out)
}

# reduce confidences to one row per reduction period
reduce_confidences <- function(m, group_ids) {

    stopifnot(identical(rownames(m), sort(rownames(m))))
    outi <-
        split.data.frame(m, group_ids) |> 
        lapply(X=_, function(mm) {
            mm <-
                # sort descending and fill forwards (backwards in chronological time)
                sort_matrix_by_rownames(m=mm, decreasing = TRUE) |> 
                vutils::locf(x=_) |> 
                # sort ascending and fill forwards
                sort_matrix_by_rownames(m=_, decreasing = FALSE) |> 
                vutils::locf(x=_)
            
            # grab the second group with dates
            dates <-
                as.Date(gsub("^(.*?)(\\d{4}-\\d{2}-\\d{2})$", "\\2", rownames(mm)))
            # Compute weights across all years
            weights <- date_to_weights_across_years(dates)
                        
            # Output is matrix with one row with weighted confidences
            outM <- matrix(
                data = apply(mm, 2, function(v) {
                    weighted.mean(v, weights = weights)}), 
                nrow = 1, 
                dimnames = list(rownames(mm)[1], colnames(mm)))
            stopifnot(is.matrix(outM))
            return(outM)

        }) |> 
        do.call(rbind, args=_) |>  
        sort_matrix_by_rownames(m=_) |>  
        round(x=_, digits = 3)

    is.na(outi) <- is.na(outi)

    # Output is a matrix with confidence weights per reduced date
    stopifnot(nrow(outi) == length(unique(group_ids)))
    return(outi)
}

# Top-level functions inside main
reduce_wdata <- function(wdata, group_index) {
	stopifnot(rownames(wdata) == names(group_index))
    interpolated_wdata <- 
        interpolate_within_groups(wdata, group_index) |> 
		sort_matrix_by_rownames(m=_)

    reduced_data <- reduce_groups(interpolated_wdata, group_index)
    colnames(reduced_data) <- sub("code_", "", colnames(reduced_data))
    return(reduced_data)
}

reduce_conf <- function(conf_mat, group_index) {
	stopifnot(rownames(conf_mat) == names(group_index))
    reduced_conf <- 
        reduce_confidences(conf_mat, group_index) |> 
		sort_matrix_by_rownames(m=_)

    colnames(reduced_conf) <- sub("conf_", "", colnames(reduced_conf))
    return(reduced_conf)
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

    stopifnot(identical(rownames(input.data$wdata), rownames(input.data$conf_mat)))

    # If a rows contains nothing but NAs, this script fails. From adding January first,
    # this can be the case for a small amount of observations.
    onlyNAs <- apply(input.data$wdata, 1, allNA)
    if (any(onlyNAs)) {
        stopifnot(length(onlyNAs) == nrow(input.data$wdata))
        stopifnot(length(onlyNAs) == nrow(input.data$conf_mat))
        info(sprintf("%s rows are removed because they only contain NAs.", 
            sum(onlyNAs)))

        # Remove these rows for both code and confidences
        # -- we can use the same index because they have the same structure 
        input.data$wdata <- input.data$wdata[!onlyNAs, ]
        input.data$conf_mat <- input.data$conf_mat[!onlyNAs, ]
    }

    info(sprintf("Reducing for %s", TASK_NAME))

	# Sort
	input.data$wdata <- sort_matrix_by_rownames(input.data$wdata)
	input.data$conf_mat <- sort_matrix_by_rownames(input.data$conf_mat)
    # Append code_ and conf_ to colnames of wdata and conf_mat
    input.data <- rename_cols_to_work(input.data)
    # Define stable coding periods as group_index
    group_index <- identify_reduction_groups(input.data$wdata)
    # Reduce wdata to stable information periods
    wdata <- reduce_wdata(input.data$wdata, group_index)
    # Reduce confidence to weights per coder across reduced dates
    conf <- reduce_conf(input.data$conf_mat, group_index)
    # Order wdata and conf after wdata order
    input.data$wdata <- sort_matrix_by_rownames(wdata)
    input.data$conf_mat <- sort_matrix_by_rownames(conf)
    checks(input.data)

    return(input.data)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    input.data <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]

    # Run
    collectedInputs <- named_list(input.data)
    out <- setNames(list(do.call(main, collectedInputs)), TASK_NAME)
    stopifnot(!is.null(rownames(out[[TASK_NAME]]$wdata))) 
    write_file(out, OUTFILE, dir_create = TRUE)

} else {
    # Tests
    testthat::test_file("~/proj/vdemds/module_unit_tests/mm_prep/test_reduce.R") %>%
		check_test_file_output()
}
