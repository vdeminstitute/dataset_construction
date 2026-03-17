#!/usr/bin/env Rscript

library(parallel)
library(rstan)
library(MASS)
library(vbase)

sessionInfo()

ITER <- 80000
BURN <- 1000 * (ITER / 10000)
THIN <- 20 * (ITER / 10000)
CHAINS <- 8
CORES <- CHAINS

# Load input file 
input <- readRDS(file.path("input", "int_input.rds"))
stopifnot(nrow(input) != 0)
manifest <- input[, !(names(input) %in% c("country_id", "country_text_id", "historical_date", "year"))]

# Load model script 
model_path <- file.path("stan", "integrity.stan")

# Report some info
info(sprintf("START: %s", Sys.time()))
info(sprintf("INFILE: %s", file.path("input", "int_input.rds")))
info(sprintf("ITER: %s", ITER))
info(sprintf("BURN: %s", BURN))
info(sprintf("THIN: %s", THIN))
info(sprintf("CHAINS: %s", CHAINS))
## Priors on the diag
a.prior <- rep(0.01, ncol(manifest))
b.prior <- rep(0.01, ncol(manifest))

## Fit model
data.stan <- list(N=nrow(manifest), J=ncol(manifest),
    manifest=manifest, a=a.prior, b=b.prior)

post.stan <- stan(file = model_path, data=data.stan, iter=ITER,
    warmup=BURN, thin=THIN, chains=CHAINS, cores=CORES)

info(sprintf("%s chains of STAN models finished", CHAINS))

# save in case debugging necessary
saveRDS(post.stan, file.path("out", "model_output.rds"))

# Convergence checks
# Check for convergence for each parameter, report if more than 5% of Rhat > 1.01
pars <- post.stan@model_pars
pars_conv <- setNames(vector(mode = "list", length = length(pars)), pars)
for (p in pars) {
    Rhat <- summary(post.stan, pars = p)$summary[, "Rhat"]
    stat <- mean(Rhat > 1.01)
    
    info(sprintf("Convergence for %s: %.2f of Rhat > 1.01", p, stat))
    
    pars_conv[[p]] <- list(stat = stat, Rhat = Rhat)

    if (stat > 0.05) {
        # Format to %s and %f with two digits
        vbase::warn(sprintf("Convergence failed for %s: %.2f of Rhat > 1.01", p, stat))
    }
}

## Grab bits of model
out <- list()
out$phi <- as.matrix(post.stan, pars="phi")
out$lambda <- as.matrix(post.stan, pars="Lambda")
out$psi <- as.matrix(post.stan, pars="Psi_diag")

# Save convergence statistics
out$convergence_stats <- pars_conv

# calculate model estimates
phi <- pnorm(out$phi)
out$phi.median <- apply(phi, 2, median)

# create cy version and use pnorm to scale between 0 and 1
cy <- data.frame(
  country_id = input$country_id, 
  country_text_id = input$country_text_id, 
  historical_date = input$historical_date, 
  year = input$year, 
  v2x_electoral_integrity = out$phi.median,
  v2x_electoral_integrity_sd = apply(phi, 2, sd),
  v2x_electoral_integrity_codelow = apply(phi, 2, quantile, probs = 0.16),
  v2x_electoral_integrity_codehigh = apply(phi, 2, quantile, probs = 0.84)
)

out$cy <- cy
out$model <- paste0(readLines(model_path), collapse = "\n")
out$ITER <- ITER

output <- list()
output[[ "v2x_electoral_integrity" ]] <- out
saveRDS(output, file.path("out", "v2x_electoral_integrity.rds"))
print("Model output saved")