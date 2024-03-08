#!/usr/bin/env Rscript

# ==========================================================================
# Generate raw posterior file for v2x_genpp.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
make_genpp <- function(v2pepwrgen, v2lgfemleg, combined_names, utable) {
    stopifnot(identical(rownames(v2lgfemleg), rownames(v2pepwrgen)))
    # Normalize
    v2lgfemleg <- scale(v2lgfemleg)
    post.genpp <- pnorm((v2pepwrgen + v2lgfemleg[,, drop = T]) / 2)
    return(post.genpp)
}

main <- function(v2pepwrgen, femleg, lgbicam, utable, TASK_NAME) {
    v2pepwrgen %<>% mm_stretch_z_sample(., utable)

    # Set v2lgfemleg to 0 when v2lgibcam is missing or zero.
    femleg %<>% clean_by_utable(utable)
    lgbicam %<>% clean_by_utable(utable)
    nonc.df <-
        full_join_vdem(femleg, lgbicam) %>%
		interpolate_components(., "v2lgbicam", utable, coder_level = FALSE) %>% 
        mutate(v2lgfemleg =
            ifelse(!is.na(v2lgbicam) & v2lgbicam == 0, 0, v2lgfemleg)) %>%
        select(-v2lgbicam) %>%
        add_country_cols(country) %>%
        clean_by_utable(utable) %>%
        as.data.frame(stringsAsFactors = F)
    rownames(nonc.df) <- with(nonc.df, paste(country_text_id, historical_date))
    v2lgfemleg <- data.matrix(nonc.df[, "v2lgfemleg", drop = FALSE])

    # Stretch to combined dates and calculate index
    ll <- stretch_combined(named_list(v2pepwrgen, v2lgfemleg), utable)
    make_genpp(ll$v2pepwrgen, ll$v2lgfemleg) %>%
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
    utable <- load_country_unit()
    country <- load_country()
    objs <- find_dep_files(TASK_ID, db)
    # A and A* variables
    femleg <- objs[["v2lgfemleg"]][["v2lgfemleg"]]$cd
    lgbicam <- objs[["v2lgbicam"]][["v2lgbicam"]]$cd
    # MM
    v2pepwrgen <- objs[["v2pepwrgen"]]

    # Run
    collectedInputs <- named_list(v2pepwrgen, femleg, lgbicam, utable, TASK_NAME)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)

    } else {
        testthat::test_file("~/proj/vdemds/module_unit_tests/hli/test_genpp.R")
}
