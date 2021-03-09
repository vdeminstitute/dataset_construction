#!/usr/bin/env Rscript

library(dplyr)
library(coda)
library(MASS)
library(rjags)
library(parallel)
library(vutils)

INDEX <- Sys.getenv("VARIABLENAME")
inputs <- read_file(file.path("z.sample", INDEX %^% ".rds"))
COMPONENTS <- inputs[[INDEX]]$components
utable <- inputs[[INDEX]]$country_unit
frassoc <- INDEX == "v2x_frassoc_thick"
n_obs <- inputs[[INDEX]]$n_obs
elec_regimes <- inputs[[INDEX]]$elec_regimes
script <- inputs[[INDEX]]$script
vars.ll <- inputs[[INDEX]]$z.samples
dropped_dates <- inputs[[INDEX]]$dropped_dates
elecreg_cy <- inputs[[INDEX]]$elecreg_cy
R_SEED <- sample(1000000, 1)
RNGkind("L'Ecuyer-CMRG")
set.seed(R_SEED)
info("Seed: " %^% R_SEED)


sessionInfo()

OUTDIR <- "out"
SAVE.NAME <- INDEX
VARS <- COMPONENTS

ITER <- 600L
BURNIN <- 10000L
MCMC <- inputs[[INDEX]]$MCMC
THIN <- inputs[[INDEX]]$THIN
PSEUDOCHAINS <- 8L

sprintf("%d runs with %d sampling iterations, %d burnin, and %d thin",
        ITER, MCMC, BURNIN, THIN) %>% info

utable_names <- with(utable, paste(country_text_id, historical_date))

n_pars <- length(VARS)

sprintf("Found %d total obs", n_obs) %>% info

inits.ll <- lapply(1:PSEUDOCHAINS, function(i) {
    list(gamma = mvrnorm(n_pars, c(0, 0), diag(.01, 2)),
         omega = runif(n_pars, 0, 10),
         xi = rnorm(n_obs, 0, 1))
})

posteriors <- mclapply(1:ITER, function(i) {
    info("Running model " %^% i)
    full.ma <- do.call(cbind, lapply(vars.ll, function(ma) ma[, i]))


    if (frassoc) {
        # We want to downweight
        # v2elmulpar and v2psoppaut when there's no electoral
        # regime. Transform by the normal CDF so we can force those point
        # estimates to bottom of the scale (ie, 0).
        full.ma[, "v2elmulpar"] <- pnorm(full.ma[, "v2elmulpar"])
        full.ma[, "v2psoppaut"] <- pnorm(full.ma[, "v2psoppaut"])

        full.ma[!is.na(full.ma[, "v2psbars"]) & is.na(full.ma[, "v2psoppaut"]), "v2psoppaut"] <- 0
        full.ma[!rownames(full.ma) %in% elec_regimes, "v2elmulpar"] <- 0
    }


    input.data <- list(n = nrow(full.ma), # n_obs
                      p = ncol(full.ma), # n_pars
                      y = full.ma)

    # Divide total runs into 8 groups, each gets same initial values
    inits <- inits.ll[[findInterval(i, seq(ITER/PSEUDOCHAINS + 1, ITER, ITER/PSEUDOCHAINS)) + 1]]

    model <- jags.model(file = script, data = input.data,
                        inits = inits, quiet = T)

    update(model, BURNIN)
    mcmc <- coda.samples(model, c("xi", "gamma", "omega"), MCMC, THIN)

    b <- grepl("xi", colnames(mcmc[[1]]), perl = T)
    colnames(mcmc[[1]])[b] <- rownames(full.ma) %^% "_" %^% colnames(mcmc[[1]])[b]

    as.mcmc(mcmc)
}, mc.cores = detectCores(), mc.preschedule = F)
mc_assert(posteriors)

info("JAGS models finished")

###
# Check for convergence for each parameter by dividing the runs into
# eight pseudo "chains" and then run the Gelman & Rubin diagnostic.
cuts <- findInterval(1:ITER, seq(ITER/PSEUDOCHAINS + 1, ITER, ITER/PSEUDOCHAINS))
gelman <- split(posteriors, cuts) %>%
    lapply(function(ll) do.call(rbind, ll) %>% as.mcmc) %>%
    mcmc.list %>%
    gelman.diag(autoburnin = F, multivariate = F)

g <- gelman$psrf
for (p in c("xi", "gamma...1", "gamma...2", "omega")) {
    if (!mean(g[grepl(p, rownames(g)), 1] > 1.1) <= .05) {
        warn("Convergence check failed for " %^% p)
        info("Proportion above 1.1: " %^% mean(g[grepl(p, rownames(g)), 1] > 1.1))
    }
}

output <- list()
output$mcmc_posteriors <- posteriors
output$R_SEED <- R_SEED
output$ITER <- ITER

###
# Extract `xi`, our latent factor, and combine together all
# the runs
combined.posterior <- lapply(posteriors, function(o)  {
    m <- as.matrix(o)

    out <- m[, grepl("xi", colnames(m), perl = T)]
    colnames(out) <- sub("_xi.*$", "", colnames(out), perl = T)

    out
}) %>% do.call(rbind, .)

# Finally, add the rows where we had >50% missingness back as NA
combined.posterior <- add_empty_cols(combined.posterior, dropped_dates)

full_names <- 
    union(inputs[[INDEX]]$country_dates, colnames(combined.posterior)) %>% 
    sort
sprintf("Found %d expanded country-dates", length(full_names)) %>% info

###
# Thin our `xi` posteriors once more for the HLIs. We're only going to
# grab 1800 draws to match the dimensions of our z.sample files when
# constructing the HLIs.
if (nrow(combined.posterior) < 1800)
    stop("Too few draws in posterior object")

# Stretch the thinned posteriors since we need to clean at least
# frefair according to elecreg.
idx <- seq(1, by = nrow(combined.posterior) / 1800, length.out = 1800)

###
# Summarise and generate point estimates at CD & CY-level.
# For v2xel_frefair we fill the whole utable country-dates 
# so that we get data even if the two non-els components are missing.
# But we stretch only when elecreg is 1.
if (INDEX == "v2xel_frefair") {
    full_names <- union(utable_names, full_names) %>% sort
    thin.ma <- t(combined.posterior[idx, ]) %>%
        scale %>%
        stretch(full_names, utable = utable, rule_366 = FALSE, 
                elecreg_cy = elecreg_cy) %>%
        pnorm
    final_cd.df <-
        combined.posterior %>%
        t %>%
        scale %>%
        t %>%
        dist_summary(full_names, utable = utable, elecreg_cy = elecreg_cy)
} else {
    thin.ma <- t(combined.posterior[idx, ]) %>%
        scale %>%
        stretch(full_names, utable = utable, rule_366 = FALSE) %>%
        pnorm
    final_cd.df <-
        combined.posterior %>%
        t %>%
        scale %>%
        t %>%
        dist_summary(full_names, utable = utable)
}

output$thin_post <- thin.ma

b <- vapply(final_cd.df, is.numeric, logical(1))
final_cd.df[, b] <- lapply(final_cd.df[, b], pnorm)
output$cd <- fix_stat_columns(final_cd.df, INDEX)
final_cy.df <- cy.day_mean(final_cd.df, historical_date, country_text_id, mc.cores = 1)
output$cy <- fix_stat_columns(final_cy.df, INDEX)

###
# write factor loadings as well
combined.posterior <- lapply(posteriors, function(o)  {
    m <- as.matrix(o)
    m <- m[, grepl("gamma|omega", colnames(m))]
    return(m)
}) %>% do.call(rbind, .)

colnames(combined.posterior)[
    grepl("gamma\\[\\d+\\,1\\]", colnames(combined.posterior))] <-
    paste0(VARS, "_intercept")

colnames(combined.posterior)[
    grepl("gamma\\[\\d+\\,2\\]", colnames(combined.posterior))] <-
    paste0(VARS, "_slope")

colnames(combined.posterior)[
    grepl("omega", colnames(combined.posterior))] <-
    paste0(VARS, "_uniqueness")

combined.posterior %<>% as.data.frame(stringsAsFactors = F)

fu <- function(x) {`^`(x = x, y = 2)}

res <- combined.posterior %>%
    dplyr::summarize_all(list(median)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("_uniqueness")), fu)

output$country_dates <- inputs[[INDEX]]$country_dates
output$factors <- res
output$model_code <- paste0(readLines(script), collapse = "\n")

outout <- list()
outout[[INDEX]] <- output
write_file(outout,
           file.path(OUTDIR, INDEX %^% ".rds"),
           dir_create = T)

info("Finished!")

# -*-R-*-
