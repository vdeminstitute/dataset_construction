#!/usr/bin/env Rscript

# ==========================================================================
# Creates raw mode of nonreduced, interpolated C-vars. Reported
# in the final DS as v2*_mode.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
compare_supgroup <- function(values, country_text_id, historical_date, row_means) {
    df <- row_means[row_means$country_text_id == country_text_id &
                    row_means$historical_date == historical_date, ]
    var <- grep("_\\d+$", names(df), value = TRUE)[1] %>% gsub("_\\d+", "", .)
    cols <- var %^% "_" %^% values
    df <- df[, cols]
    out <- colnames(df)[which.max(unlist(df))]
    gsub("^.*_(\\d+)$", "\\1", out)
}

calc_mode <- function(df, row_means) {
    m <- df - 1

    table_by_row_and_sort <- function(x) {    
        out_vector <- sort(table(x), decreasing = TRUE)
        return(out_vector)
    
    }

    scores <- apply(m, 1, table_by_row_and_sort)

    out <- lapply(1:length(scores), function(i) {

        s <- scores[[i]]

        if (length(s) == 1)
            return(names(s))

        if (s[1] != s[2])
            return(names(s)[1])

        if (s[1] == s[2]) {
            if (is.null(row_means))
                return("4")

            return(compare_supgroup(names(s)[s == s[1]],
                                    substr(names(scores)[i], 1, 3),
                                    substr(names(scores)[i], 5, 14),
                                    row_means))
        }
    })
    names(out) <- names(scores)
    out <- as.numeric(out)
    names(out) <- names(scores)
    return(out)
}

mode_df <- function(out, TASK_NAME) {
    df <- data.frame(country_text_id = substr(names(out), 1, 3),
                 historical_date = as.Date(substr(names(out), 5, 14)))
    df[[TASK_NAME]] <- out
    return(df)
}

cy_df <- function(df) {
    # CY Aggregate by last:
    dfcy <-
        df %>%
        arrange(country_text_id, historical_date) %>%
        mutate(year = to_year(historical_date)) %>%
        group_by(country_text_id, year) %>%
        filter(row_number() == n()) %>%
        ungroup()
}

main <- function(df, objs, TASK_NAME) {

	info("Removing rows where there are only NAs (added by Jan01)!")
	all_na_rows <- apply(df, 1, allNA) %>% which()
	if (length(all_na_rows) > 0L) {
		info("Removing " %^% length(all_na_rows) %^% " rows!")
		df <- df[-all_na_rows, , drop = FALSE]
	}

    deps <- objs[grepl("_\\d", names(objs))]
    if (length(deps) > 0) {
        dep_nms <- names(deps)
        row_means <- lapply(dep_nms, function(dname) {
            deps[[dname]][[dname]][["cd"]]}) %>%
            Reduce(function(x, y) {
            full_join(x, y, by = c("country_text_id", "historical_date"))}, x = .)
    } else {
        row_means <- NULL
    }

    out <- calc_mode(df, row_means)
    df <- mode_df(out, TASK_NAME)
    dfcy <- cy_df(df)
    return(list(cy = dfcy, cd = df))
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    objs <- find_dep_files(TASK_ID, db)
    df <- objs[[TASK_NAME]][[TASK_NAME]][["wdata"]]
    
    # Run
    collectedInputs <- named_list(df, objs, TASK_NAME)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done with calculating mode for " %^% TASK_NAME)
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/mm_prep/test_raw_mode.R") %>%
		check_test_file_output()
}
