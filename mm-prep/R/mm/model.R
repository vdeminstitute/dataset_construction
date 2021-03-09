#!/usr/bin/env Rscript
#
# IRT Model R Script

options(scipen = 10)
library(dplyr)
library(rstan)
library(tools)
library(vutils)

sessionInfo()

rstan_options(auto_write = FALSE)

VARIABLE <- Sys.getenv("VARIABLENAME")
ITER   <- as.numeric(Sys.getenv("ITER"))
OUTDIR <- "out/"
INFILE <- file.path("mm", VARIABLE %^% ".rds")
R_SEED <- Sys.getenv("R_SEED")
STAN_SEED <- Sys.getenv("STAN_SEED")
output <- list()

# Grab or set seeds for R and STAN
if (STAN_SEED == "0")
    STAN_SEED <- sample(1000000, 1)
if (R_SEED == "0")
    R_SEED <- sample(1000000, 1)

R_SEED <- as.integer(R_SEED)
STAN_SEED <- as.integer(STAN_SEED)

OUTDIR_ <- file.path(OUTDIR, as.character(ITER/1000) %^% "k")
dir.create(OUTDIR_, recursive = T)

stopifnot(file.exists(INFILE), file.access(INFILE, mode = 4) == 0)
stopifnot(dir.exists(OUTDIR_), file.access(OUTDIR_, mode = 2) == 0)
stopifnot(is.numeric(ITER), ITER > 0, length(ITER) == 1)

NAME   <- VARIABLE
WARMUP <- 1000 * (ITER / 10000)
THIN   <- 20 * (ITER / 10000)
CHAINS <- 8
CORES  <- CHAINS




set.seed(R_SEED)

info("START: " %^% Sys.time())
info("NAME: " %^% NAME)
info("INFILE: " %^% INFILE)
info("OUTDIR: " %^% OUTDIR_)
info("ITER: " %^% ITER)
info("STAN_SEED: " %^% STAN_SEED)
info("R_SEED: " %^% R_SEED)
info(sprintf("WARMUP: %f THIN: %f CHAINS: %f", WARMUP, THIN, CHAINS))

###
# Let's start with loading some data and excluding vignettes from our
# initial priors.
ll <- read_file(INFILE)[[VARIABLE]]
utable <- ll$utable
model_input <- ll$model_input 
country_date_translation <- ll$country_date_translation 
coder_translation <- ll$coder_translation
country_dates <- ll$country_dates
main_country_translation <- ll$main_country_translation

# For binary vars, transform data to 0, 1 & load matching stan code
if (model_input$K == 2) {
    stanfile <- "bin_mcc.stan"

    data <- list(K = model_input$K,                  # Number of answer categories [1]
            J = model_input$J,                       # Number of coders [1]
            C = model_input$C,                       # Number of coder main countries [1]
            N = model_input$N,                        # Number of unique country-dates [1]
            n_obs = model_input$n_obs,               # Number of long-format observations [1]
            cdata_id = model_input$cdata_id,         # Main country coded (1..C) matched for rater_id 1..J [J]
            wdata = model_input$code,               # Coder-submitted ratings (with baseline 0) [n_obs]
            rater_id = model_input$rater_id,         # rater_id (factor of coder_id's to be from 1 to J) [n_obs]
            country_date_id = model_input$country_date_id, # country_date_id factor of country-date (1 to N) [n_obs]
            gsigmasq = 0.2,
            gsigmasqc = 0.2,
            mc = model_input$mc)                 # Priors matched for country_date_id 1..N [N]

    data$wdata <- data$wdata - 1
} else {
    stanfile <- "quasilda4.stan"

    data <- list(K = model_input$K,                      # Number of answer categories [1]
                 J = model_input$J,                      # Number of coders [1]
                 C = model_input$C,                      # Number of main countries [1]
                 N = model_input$N,                      # Number of unique country-dates [1]
                 n_obs = model_input$n_obs,              # Total number of long-format observations [1]
                 rater_state = model_input$cdata_id,     # Main country coded (1..C) matched for rater_id 1..J [J]
                 y = model_input$code,                   # Coder-submitted ratings (with baseline 1) [n_obs]
                 j_id = model_input$rater_id,            # rater_id (factor of coder_id's to be from 1 to J) [n_obs]
                 sy_id = model_input$country_date_id,    # country_date_id factor of country-date (1 to N) [n_obs]
                 gsigmasq = 0.2,
                 gsigmasqc = 0.2,
                 mc = model_input$mc)                    # Priors matched for country_date_id 1..N [N]
}

posterior.sim <- stan(file = file.path("stan", stanfile),
                      data = data,
                      iter = ITER,
                      thin = THIN,
                      chains = CHAINS,
                      cores = CORES,
                      warmup = WARMUP,
                      init_r = 0.1,
                      control = list(max_treedepth = 2e1),
                      pars = "Z_star",
                      include = F,
                      seed = STAN_SEED)

get_elapsed_time(posterior.sim)

output$posterior <- posterior.sim
output$model_code <- get_stancode(posterior.sim)
output$model_input <- data
output$script_input <- model_input
output$country_dates <- country_dates
output$country_date_translation <- country_date_translation 
output$coder_translation <- coder_translation
output$main_country_translation <- main_country_translation
output$missing <- ll$missing
output$ITER <- ITER
output$R_SEED <- R_SEED
output$STAN_SEED <- STAN_SEED


pars <- c("Z",        # Estimated latent variable
          "gamma_mu", # Universial thresholds
          "beta",     # Reliability scores
          "gamma",    # Coder-specific thresholds
          "gamma_c")  # Country-specific thresholds

###
# Before we start summarising and creating our different variable
# versions, check convergence (mean(Rhat > 1.01) <= 0.05)
for (p in pars) {
    Rhat <- summary(posterior.sim, pars = p)$summary[, "Rhat"]
    stat <- mean(Rhat > 1.01)

    if (!stat <= 0.05) {
        warn("Convergence failed for: " %^% paste0(p, ":") %^% stat)
    }
}

###
# Extract each parameter as a matrix for later use.
pars.matrices <- lapply(pars, function(p) {
    # Keep this for debugging purposes. Seriously, you really want
    # this info in your log files.
    sprintf("Extracting %s", p) %>% info

    m <- as.matrix(posterior.sim, pars = p)
    if (p == "gamma_mu")
        return(m)

    colnames(m) <- switch(p,
        "gamma" = lapply(1:(model_input$K - 1), paste,
            coder_translation$coder_id, sep = ".") %>% unlist,
        "gamma_c" = lapply(1:(model_input$K - 1), paste,
            main_country_translation$main_country_id, sep = ".") %>% unlist,
        "beta" = coder_translation$coder_id,
        country_date_translation$country_date)

    return(m)
})

names(pars.matrices) <- pars
output$post.sample <- list()

###
# Save half of our draws as samples to be publically released
for (p in names(pars.matrices)) {

    m <- pars.matrices[[p]]
    out <- t(m[sample(nrow(m), nrow(m) / 2) %>% sort, ])

    # Add the missing country-dates so that we can keep track of them
    # for the BFAs
    if (p == "Z")
        out <- add_empty_rows(out, ll$missing)

    output$post.sample[[tolower(p)]] <- out
}


###
# Finally, let's create the different variable versions that go into
# the public DS. For each var type, create both a country-date and
# aggregated country-year.
#
# Variable versions:
#     1. post.summary -> Latent trait estimates
#     2. osp -> Linearized ordinal scale
#     3. ord -> Ordinal estimates
summarise_t <- partial(dist_summary, expanded.names = country_dates,
            utable = utable)

for (type in c("post.summary", "osp", "ord")) {
    sprintf("Generating %s", type) %>% info

    cd <- switch(type,
               "post.summary" = summarise_t(pars.matrices$Z),
               "osp" = osp(pars.matrices$Z, pars.matrices$gamma_mu) %>% summarise_t,
               "ord" = ord(pars.matrices$Z, pars.matrices$gamma_mu) %>% summarise_t)

    # If creating ordinal estimates, truncate decimals
    if (type == "ord")
        cd$median <- floor(cd$median)

    # OBS! For ords, aggregate by taking the last observation,
    # otherwise day-weighted mean
    if (type != "ord") {
        cy <- cy.day_mean(cd, historical_date, country_text_id)
    } else {
        cy <- transform(cd, year = to_year(historical_date), historical_date = NULL) %>%
            aggregate(. ~ country_text_id + year, ., partial(tail, n = 1))
    }

    output[[type]] <- list(cd = cd, cy = cy)
}


# Generate beta summaries for the disaggregated dataset
####
output$b.summary <- b_summary(pars.matrices$beta)

# Fix columns
###
output$cd <- Reduce(
    partial(full_join,
            by = c("country_text_id","historical_date")),
    list(fix_stat_columns(output$post.summary$cd, VARIABLE),
         fix_stat_columns(output$osp$cd, VARIABLE %^% "_osp"),
         fix_stat_columns(output$ord$cd, VARIABLE %^% "_ord")))

output$cy <- Reduce(
    partial(full_join,
            by = c("country_text_id","year")),
    list(fix_stat_columns(output$post.summary$cy, VARIABLE),
         fix_stat_columns(output$osp$cy, VARIABLE %^% "_osp"),
         fix_stat_columns(output$ord$cy, VARIABLE %^% "_ord")))


# Write file
write_file(setNames(list(output), VARIABLE), 
    file.path(OUTDIR_, VARIABLE %^% ".rds"), dir_create = T)
info("Done with " %^% NAME %^% " at " %^% ITER %^% " iterations.")
info("END: " %^% Sys.time())
