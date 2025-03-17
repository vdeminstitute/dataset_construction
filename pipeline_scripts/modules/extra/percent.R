#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Percent.R aggregated coder-level to the country-date level by a bootstraping procedure.
# ------------------------------------------------------------------------------

suppressMessages(library(boot))
suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))
R_SEED <- sample(1000000, 1)
set.seed(R_SEED)
info("Seed: " %^% R_SEED)

#
# Functions
# --------------------------------------------------------------------------
make_bootstrapped <- function(input.data, BOOTS) {
    
    ll <- lapply(1:nrow(input.data$wdata), function(i) {
        r <- input.data$wdata[i, ]
        dat <- r[!is.na(r)]
        boot::boot(
            data=dat,
            statistic = function(rep, index) { mean(rep[index]) },
            R = BOOTS)
    })
    return(ll)
}

sample_means <- function(ll, input.data) {
    # Save the sample means for the BFAs
    ma <- lapply(ll, function(boots) as.vector(boots$t)) %>%
        do.call(rbind, .)

    rownames(ma) <- rownames(input.data$wdata)
    # Add the gap years
    ma <- add_empty_rows(ma, input.data$missing)
    
    return(ma)
}

invert_v2clsnlpct <- function(ma, TASK_NAME) {
    # v2clsnlpct needs to be inverted for v2xeg_eqprotec
    if (TASK_NAME == "v2clsnlpct") {
        info("Inverting v2clsnlpct")
        ma <- 100 - ma
    }
    return(ma)
}

point_estimates <- function(ll) {
    # Create the point estimate for the DS
    raw.df <- lapply(ll, function(boots) {
        with(boots, list(t0, sd(t), quantile(t, 0.5))) %>%
            setNames(c("mean", "sd", "median"))
    }) %>% bind_rows
}

make_bootstrapped_df <- function(raw.df, input.data) {
    bootstr <-
        data.matrix(mutate(raw.df,
            codelow95 = mean - (2 * sd),
            codehigh95 = mean + (2 * sd),
            codelow68 = mean - sd,
            codehigh68 = mean + sd)
            )

    bootstr <- bootstr[complete.cases(bootstr), ]
    bootstr[bootstr < 0] <- 0
    bootstr[bootstr > 100] <- 100

    stopifnot(!is.na(bootstr))

    rownames(bootstr) <- rownames(input.data$wdata)
    return(bootstr)
}

cd_df <- function(bootstr, input.data, utable) {
    cd.df <-
        stretch(bootstr, input.data$country_dates, utable = utable) %>%
        as.data.frame() %>%
        mutate(country_text_id = row.names(.) %>% get_text_id(),
            historical_date = row.names(.) %>% get_date()) %>%
        arrange(country_text_id, historical_date)

    row.names(cd.df) <- NULL  
    return(cd.df)
}

cy_df <- function(cd.df) {
    # Aggregate CY version
    cy.df <- cy.day_mean(cd.df, historical_date, country_text_id)   
}

main <- function(input.data, BOOTS, TASK_NAME, utable) {
    info(sprintf("Bootsraping %s using %d draws", TASK_NAME, BOOTS))

    ll <- make_bootstrapped(input.data, BOOTS)
    ma <- invert_v2clsnlpct(sample_means(ll,  input.data), TASK_NAME)
    z <- scale_matrix(ma)
    raw.df <- point_estimates(ll)
    bootstr <- make_bootstrapped_df(raw.df, input.data)
    cd.df <- cd_df(bootstr,  input.data, utable)
    cy.df <- fix_stat_columns(cy_df(cd.df), TASK_NAME)
    cd.df <- fix_stat_columns(cd.df, TASK_NAME)
    return(list(
        cd = cd.df,
        cy = cy.df,
        country_dates = input.data$country_dates,
        post.sample = list(z = z)))
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
    qtable <- subset(load_qtable(), name == TASK_NAME)
    utable <- load_country_unit()
    BOOTS <- 1800
    
    # Run
    collectedInput <- named_list(input.data, BOOTS, TASK_NAME, utable)
    setNames(list(do.call(main, collectedInput)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Finished: " %^% TASK_NAME)

    } else {
        testthat::test_file("~/proj/vdemds/module_unit_tests/extra/test_percent.R")
    }
