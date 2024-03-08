#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2xel_locelec.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
combine_rownames <- function(nonc.ma, prob.v2ellocpwr) {
    combined_names <- union(rownames(nonc.ma), rownames(prob.v2ellocpwr))
    sprintf("Found %d combined country-dates", length(combined_names)) %>% info
    return(combined_names)
}

stretch_comps <- function(nonc.ma, prob.v2ellocpwr, combined_names, utable) {
    nonc.ma <- stretch(nonc.ma, combined_names, preserve.na = FALSE, 
                       interpolate = FALSE, utable = utable)
    prob.v2ellocpwr <- stretch(prob.v2ellocpwr, combined_names, utable = utable)

    stopifnot(identical(rownames(nonc.ma), rownames(prob.v2ellocpwr)))

    out <- list(nonc.ma = nonc.ma, prob.v2ellocpwr = prob.v2ellocpwr)
    return(out)
}

calc_locelec <- function(comp_list) {
    stopifnot(c("nonc.ma", "prob.v2ellocpwr") %in% names(comp_list),
        is.list(comp_list),
        length(comp_list) == 2)
    nonc.ma <- comp_list[["nonc.ma"]]
    locelec <- comp_list[["prob.v2ellocpwr"]] * nonc.ma[, "v2ellocelc"]
    no_reggov <- !is.na(nonc.ma[, "v2ellocgov"]) & nonc.ma[, "v2ellocgov"] == 0
    locelec[no_reggov, ] <- 0
    return(locelec)
}


main <- function(ellocelc, ellocgov, prob.v2ellocpwr, utable, country, TASK_NAME) {
    
    # Prepare mm-component
    prob.v2ellocpwr %<>% mm_stretch_z_sample(., utable) %>% pnorm

    # Prepare A variables
    nonc.df <- full_join_vdem(ellocelc, ellocgov) %>%
        add_country_cols(country) %>%
        clean_by_utable(utable) %>%
        mutate(v2ellocelc = case_when(
            v2ellocelc == 0 ~ 0,
            v2ellocelc %in% c(1, 3) ~ 1,
            v2ellocelc %in% c(2, 4) ~ 2,
            v2ellocelc == 5 ~ 3,
            TRUE ~ as.numeric(v2ellocelc)))
    rownames(nonc.df) <- with(nonc.df, paste(country_text_id, historical_date))
    nonc.ma <- data.matrix(nonc.df[, c("v2ellocelc", "v2ellocgov")])
    nonc.ma[, "v2ellocelc"] <- pnorm(nonc.ma[, "v2ellocelc"])

    # Calculate index
    combined_names <- combine_rownames(nonc.ma, prob.v2ellocpwr)
    stretch_comps(nonc.ma, prob.v2ellocpwr, combined_names, utable) %>%
        calc_locelec(.) %>%
		hli_summary(., TASK_NAME)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    country <- load_country()
    utable <- load_country_unit()
    objs <- find_dep_files(TASK_ID, db)
    # Both A,C so technically A-variables
    ellocelc <- objs[["v2ellocelc"]][["v2ellocelc"]]$cd
    ellocgov <- objs[["v2ellocgov"]][["v2ellocgov"]]$cd
    # MM
    prob.v2ellocpwr <- objs[["v2ellocpwr"]]
    
    # Run
    collectedInputs <- named_list(ellocelc, ellocgov, prob.v2ellocpwr, utable, 
                                  country, TASK_NAME)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
		write_file(., OUTFILE, dir_create = TRUE)
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/hli/test_locelec.R") 
}
