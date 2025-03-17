#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# This script run the IRT model for a given variable. The input is the output from append.R. 
#
# We save 50% draws plus summarised Z, OSP, and ORDs at country-date
# and country-year level.
# ------------------------------------------------------------------------------

options(scipen = 10)
library(dplyr)
library(rstan)
library(tools)
library(vutils)

rstan_options(auto_write = FALSE)
sessionInfo()

# Collect input to script
VARIABLE <- Sys.getenv("VARIABLENAME")
ITER   <- as.numeric(Sys.getenv("ITER"))
OUTDIR <- "out/"
INFILE <- file.path("mm", VARIABLE %^% ".rds")
R_SEED <- Sys.getenv("R_SEED")
STAN_SEED <- Sys.getenv("STAN_SEED")
output <- list()

# Grab or set seeds for R and STAN
if (STAN_SEED == "0") {
    STAN_SEED <- sample(1000000, 1)
}
if (R_SEED == "0") {
    R_SEED <- sample(1000000, 1)
}

R_SEED <- as.integer(R_SEED)
STAN_SEED <- as.integer(STAN_SEED)
OUTDIR_ <- file.path(OUTDIR, as.character(ITER/1000) %^% "k")
dir.create(OUTDIR_, recursive = TRUE)

stopifnot(file.exists(INFILE), file.access(INFILE, mode = 4) == 0)
stopifnot(dir.exists(OUTDIR_), file.access(OUTDIR_, mode = 2) == 0)
stopifnot(is.numeric(ITER), ITER > 0, length(ITER) == 1)

NAME   <- VARIABLE
WARMUP <- 1000 * (ITER / 10000)
THIN   <- 20 * (ITER / 10000)
CHAINS <- 8
CORES  <- CHAINS

# Set RNG for R
set.seed(R_SEED)

# Info about the run
info(sprintf("START: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
info(sprintf("NAME: %s", NAME))
info(sprintf("INFILE: %s", INFILE))
info(sprintf("OUTDIR: %s", OUTDIR_))
info(sprintf("ITER: %d", ITER))
info(sprintf("STAN_SEED: %d", STAN_SEED))
info(sprintf("R_SEED: %d", R_SEED))
info(sprintf("WARMUP: [%f] THIN: [%f] CHAINS: [%f]", WARMUP, THIN, CHAINS))

###
# Let's start with loading some data and excluding vignettes from our
# initial priors.
ll <- read_file(INFILE)[[VARIABLE]]
utable <- ll$utable
country <- ll$country
qtable <- ll$qtable
elecreg_cy <- ll$elecreg
model_input <- ll$model_input 
country_date_translation <- ll$country_date_translation 
coder_translation <- ll$coder_translation
country_dates <- ll$country_dates
main_country_translation <- ll$main_country_translation


# For binary vars Stan expects data \in {0,1}
if (model_input$K == 2) {
    stanfile <- "bin_mcc.stan"

    data <- list(
        K = model_input$K,                     # Number of answer categories [1]
        J = model_input$J,                          # Number of coders [1]
        C = model_input$C,                          # Number of coder main countries [1]
        N = model_input$N,                          # Number of unique country-dates [1]
        n_obs = model_input$n_obs,                  # Number of long-format observations [1]
        cdata_id = model_input$cdata_id,            # Main country coded (1..C) matched for rater_id 1..J [J]
        wdata = model_input$code - 1,               # Coder-submitted ratings (with baseline 0) [n_obs]
        rater_id = model_input$rater_id,            # rater_id (factor of coder_id's to be from 1 to J) [n_obs]
        country_date_id = model_input$country_date_id, # country_date_id factor of country-date (1 to N) [n_obs]
        gsigmasq = 0.2,
        gsigmasqc = 0.2,
        mc = model_input$mc)                 # Priors matched for country_date_id 1..N [N]

} else {
    stanfile <- "quasilda4.stan"

    data <- list(
        K = model_input$K,                      # Number of answer categories [1]
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
output$model_code <- rstan::get_stancode(posterior.sim)
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
          "gamma_mu", # Universal thresholds
          "beta",     # Rater reliability 
          "gamma",    # Coder-specific thresholds
          "gamma_c")  # Country-specific thresholds

# Potential scale reduction factor (Rhat) for each parameter:
# -- criterion for passing is currently: mean(I(Rhat > 1.01)) > 0.05
pars_conv <- setNames(vector(mode = "list", length = length(pars)), pars)
for (p in pars) {
    Rhat <- summary(posterior.sim, pars = p)$summary[, "Rhat"]
    stat <- mean(Rhat > 1.01)
    pars_conv[[p]] <- list(stat = stat, Rhat = Rhat)

    if (!stat <= 0.05) {
        # Format to %s and %f with two digits
        vbase::warn(sprintf("Convergence failed for %s: %.2f", p, stat))
    }
}

# Extract each parameter as a matrix for later use.
pars.matrices <- lapply(pars, function(p) {
    sprintf("Extracting %s", p) %>% info

    m <- as.matrix(posterior.sim, pars = p)
    if (p == "gamma_mu") {
        return(m)
    }

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

# Thin the posterior files
# -- save half of the draws (they are assumed independent)
for (p in names(pars.matrices)) {

    m <- pars.matrices[[p]]
    out <- t(m[ sort(sample(nrow(m), nrow(m) / 2)) , ])

    # Add the missing country-dates so that we can keep track of them
    # for the BFAs
    if (p == "Z") {
        out <- add_empty_rows(out, ll$missing)
    }

    output$post.sample[[tolower(p)]] <- out
}

# For each parameter and variable version, create a country-date and country-year format
# -- Variable versions:
#     1. post.summary -> Latent trait estimates
#     2. osp -> Linearized ordinal scale
#     3. ord -> Ordinal estimates
summarise_t <- partial(dist_summary, expanded.names = country_dates,
            utable = utable)

for (type in c("post.summary", "osp", "ord")) {
    info(sprintf("Generating %s", type))

    cd <- switch(type,
        "post.summary" = summarise_t(pars.matrices$Z),
        "osp" = summarise_t(osp(pars.matrices$Z, pars.matrices$gamma_mu)),
        "ord" = summarise_t(ord(pars.matrices$Z, pars.matrices$gamma_mu)))

    # If creating ordinal estimates, truncate decimals
    if (type == "ord") {
        cd$median <- floor(cd$median)
    }

	# For election-date specific variables we first stretch the country-dates
    # to prevent that historical-only variables are interpolated into the
    # contemporary era, we use utable and variable to do this
	if (isTRUE(qtable$els)) {
		cd_for_cy <- election_date_mm_stretch_country_date(
			df=cd,
            tag=VARIABLE,
            utable=utable,
            elecreg_cy=elecreg_cy,
            country=country) 
	} else {
		cd_for_cy <- cd
	}

    # For all OSP and post.summary except for ELS variables, use day-weighted mean
    # For ELS + OSP and post.summary we use the mean instead.
    if (type != "ord") {
        # For ELS variables, aggregate using mean
        if (qtable$els) {
            cy <- vbase::organiseRows(aggregate(
                . ~ country_text_id + year,
                transform(cd_for_cy, year = to_year(historical_date), historical_date = NULL),
                mean), country_text_id, year)
		# Aggregate using day-weighted mean
        } else {
            cy <- vbase::organiseRows(cy.day_mean(
                x = cd_for_cy,
                dates = historical_date,
                by = country_text_id), country_text_id, year)
        }
    } else {
		# For ordinal versions aggregate using last
        cy <- vbase::organiseRows(aggregate(
            . ~ country_text_id + year,
            transform(cd_for_cy, year = to_year(historical_date), historical_date = NULL),
            partial(tail, n = 1)), country_text_id, year)
    }

    output[[type]] <- list(cd = cd, cy = cy)
}


# Generate beta summaries for the disaggregated dataset
####
output$b.summary <- b_summary(pars.matrices$beta)

# Save convergence statistics
output$convergence_stats <- pars_conv

# Fix columns
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
    file.path(OUTDIR_, VARIABLE %^% ".rds"), dir_create = TRUE)
info("Done with " %^% NAME %^% " at " %^% ITER %^% " iterations.")
info("END: " %^% Sys.time())
