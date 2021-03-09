#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_partip.
# ==========================================================================

options(warn = 2)

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "v2x_partip")

#
# Functions
# --------------------------------------------------------------------------
make_partip <- function(prob.cspart, post.regelec, post.locelec, dd.ma) {
    locregelec <- pmax(post.regelec, post.locelec, na.rm = TRUE)
    return((prob.cspart + locregelec + dd.ma[, 1]) / 3)
}

main <- function(prob.cspart, post.regelec, post.locelec, dd.df, utable, country, 
                 VARNAME) {
    utable_date <- utable %>% mutate(historical_date = as.Date(historical_date))
  
    prob.cspart %<>% bfa_stretch_z_sample(., utable)

    # Interpolate v2xdd_dd
    dd.df %<>%
        add_country_cols(country) %>%
        clean_by_utable(utable) %>%
        select(country_id, historical_date, v2xdd_dd) %>%
        full_join(select(utable_date, country_id, country_text_id, historical_date),
                  by = c("country_id", "historical_date"))
    dd.df$v2xdd_dd[is.na(dd.df$v2xdd_dd)] <- 0
    clean_dd.df <- distinct(dd.df, country_id, country_text_id, historical_date,
                           .keep_all = TRUE)
    sprintf("Cleaned %d rows from DD data", nrow(dd.df) - nrow(clean_dd.df)) %>% info
    dd.ma <- data.matrix(clean_dd.df[, "v2xdd_dd", drop = FALSE])
    rownames(dd.ma) <- with(clean_dd.df, paste(country_text_id, historical_date))

    # Calculate index
    ll <- stretch_combined(named_list(prob.cspart, post.regelec, post.locelec, dd.ma), 
                           utable)
    make_partip(ll$prob.cspart, ll$post.regelec, ll$post.locelec, ll$dd.ma) %>%
		hli_summary(., VARNAME)
}


# Run
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    country <- load_country()
    utable <- load_country_unit()
    objs <- find_dep_files(TASK_ID, DB)
    dd.df <- objs[["v2xdd_dd"]][["v2xdd_dd"]]$cd 
    # BFAs
    prob.cspart <- objs[["v2x_cspart"]]
    # HLIs
    post.regelec <- objs[["v2xel_regelec"]][["v2xel_regelec"]]$thin_post %>%
            load_matrix
    post.locelec <- objs[["v2xel_locelec"]][["v2xel_locelec"]]$thin_post %>%
            load_matrix

    # Run
    collectedInputs <- named_list(prob.cspart, post.regelec, post.locelec, 
                                  dd.df, utable, country, VARNAME)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)

} else {
    testthat::test_file("~/proj/mm-prep/tests/hli/test_partip.R")
}
update_task_status(db = DB)
