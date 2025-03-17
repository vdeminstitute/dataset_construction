library(vutils)
library(runjags)
library(rjags)
library(parallel)

sessionInfo()

NAME <- "v2x_accountability"
ITER <- 500L
BURNIN <- 5000L
THIN <- 20L
MCMC <- 2000L # Used to determine how many samples to draw from the posterior result
WARMUP <- 5000L
CHAINS <- 4L

# Random seed for R 
R_SEED <- sample(10, 4)
RNGkind("L'Ecuyer-CMRG")
set.seed(R_SEED)
info("Seed: " %^% R_SEED)

# Load input file 
acc <- readRDS(file.path("input", "acc_input.rds"))
stopifnot(nrow(acc) != 0)
# Load model script 
model_path <- file.path("jags", "accountability_MM.txt")

# Report some info
info(sprintf("START: %s", Sys.time()))
info(sprintf("NAME: %s", NAME))
info(sprintf("INFILE: %s", file.path("input", "acc_input.rds")))
info(sprintf("ITER: %s", ITER))

# Transform input data to JAGS format 
# -- the structure of the JAGS data is hardcoded, i.e. if anything changes this must be reflected here too
hmod <- list(
    N = nrow(acc),
    b0 = rep(0, 2),
    B0 = diag(1, 2),
    yCS = cbind(acc$v2csreprss, acc$v2csprtcpt, acc$v2cseeorgs),
    yMV = cbind(acc$v2mebias, acc$v2mecrit, acc$v2merange, acc$v2mecenefm, acc$v2meharjrn,
        acc$v2meslfcen),
    yFE = cbind(acc$v2cldiscm, acc$v2cldiscw, acc$v2clacfree, acc$v2mecenefi),
    yES = acc$v2dlengage,
    yLC = cbind(acc$v2lginvstp, acc$v2lgqstexp),
    yL = acc$v2lgbicam,
    yJC = cbind(acc$v2jucomp, acc$v2juhccomp, acc$v2juhcind, acc$v2juncind),
    yIB = acc$v2lgotovst,
    yEC = acc$v2exrescon,
    yEV = cbind(acc$v2elembaut, acc$v2elembcap,	acc$v2elrgstry, acc$v2elirreg, acc$v2elintim,
        acc$v2elfrfair, acc$v2elmulpar),
    yER = acc$v2x_elecreg,
    yPE = acc$v2x_suffr,
    yO3 = acc$HoEel,
    yP = cbind(acc$v2psparban, acc$v2psbars, acc$v2psoppaut)
)

# List the parameters to save
monitorParameters <- c("betaCS", "tauCS", "tauC", "betaMV", "tauMV", "tauM", "betaFE", "tauFE", "tauF",
  "betaES", "tauES",  "betaLC", "tauLC", "betaL", "betaJC", "tauJC", "tauJ", "betaIB", "tauIB",
  "betaEC", "tauEC","betaEV", "tauEV", "betaER", "betaPE", "tauPE", "betaP",
  "tauPP", "tauP", "tauSocial", "tauHorizontal", "tauVertical", "xi")

info(sprintf("%d runs with %d sampling iterations, %d burn-in, and %d thin",
    ITER, MCMC, BURNIN, THIN))

info(sprintf("Found %d total obs", nrow(acc)))

# Estimate posteriors
hmodel1 <- run.jags(
    method = "parallel",
    model = model_path,
    monitor = monitorParameters,
    data = hmod,
    n.chains = CHAINS,
    inits = list(xi = 2 * acc$v2x_elecreg - 1),
    adapt = 5000,
    burnin = BURNIN,
    sample = ITER,
    thin = THIN,
    summarise = FALSE,
    plots = FALSE,
    modules = c("glm", "lecuyer")
)

info(sprintf("%s chains of JAGS models finished", CHAINS))
posteriorDraws1 <- as.mcmc.list(hmodel1)

write_file(posteriorDraws1, file.path("out",  sprintf("%s_%s_%s", NAME, Sys.getenv("SLURM_JOB_ID"), "_posteriors1.rds")))
write_file(hmodel1, file.path("out",  sprintf("%s_%s_%s", NAME, Sys.getenv("SLURM_JOB_ID"), "_hmodel1.rds")))

# Reset seed 
R_SEED <- sample(10, 4)
RNGkind("L'Ecuyer-CMRG")
set.seed(R_SEED)
info("Seed: " %^% R_SEED)

hmodel2 <- run.jags(
    method = "parallel",
    model = model_path,
    monitor = monitorParameters,
    data = hmod,
    n.chains = CHAINS,
    inits = list(xi = 2 * acc$v2x_elecreg - 1),
    adapt = 5000,
    burnin = BURNIN,
    sample = ITER,
    thin = THIN,
    summarise = FALSE,
    plots = FALSE,
    modules = c("glm", "lecuyer")
)

info(sprintf("%s chains of JAGS models finished", CHAINS))
posteriorDraws2 <- as.mcmc.list(hmodel2)

write_file(posteriorDraws2, file.path("out",  sprintf("%s_%s_%s", NAME, Sys.getenv("SLURM_JOB_ID"), "_posteriors2.rds")))
write_file(hmodel2, file.path("out",  sprintf("%s_%s_%s", NAME, Sys.getenv("SLURM_JOB_ID"), "_hmodel2.rds")))

posteriorDraws <- c(posteriorDraws1, posteriorDraws2) %>% as.mcmc.list()
hmodel <- mapply(c, hmodel1, hmodel2)
class(hmodel) <- "runjags"

# Check for convergence using Gelman diagnostics
gelman.diag(posteriorDraws[,1:111]) # All parameters except xi
xiRhat <- gelman.diag(posteriorDraws[, sample(112:(111 + nrow(acc)), MCMC)], multivariate = FALSE) # xi parameters
info("Proportion of R hat above 1.01: " %^% mean(xiRhat[[1]][, 2] > 1.01))

# Standardize posterior draws to have mean 0 and variance 1
idbayes <- function(x){
    mx <- apply(x, 1, mean)
    sdx <- apply(x, 1, sd)
    idb <- matrix(nrow = nrow(x), ncol = ncol(x))
    for(j in 1:nrow(x)) {
        idb[j,] <- (x[j,] - mx[j]) / sdx[j]
    }
    return(idb)
}

# Extract country-date posterior samples
xi <- idbayes(combine.mcmc(hmodel$mcmc[, 112:(111 + nrow(acc))]))

# Standard model estimates of index 
mxi <- apply(xi, 2, median) # point-estimate for index
xi68 <- HPDinterval(as.mcmc(xi), prob = .68) # credible interval for index

# Original scale predictions of index
pxi <- pnorm(xi) # Make xi vary from 0 to 1
pmxi <- apply(pxi, 2, median) # median
pxi68 <- HPDinterval(as.mcmc(pxi), prob= .68) # credible interval

# Store estimates in input object
acc$v2x_accountability <- mxi
acc$v2x_accountability_codelow <- xi68[, 1]
acc$v2x_accountability_codehigh <- xi68[, 2]
acc$v2x_accountability_osp <- pmxi
acc$v2x_accountability_osp_codelow <- pxi68[, 1]
acc$v2x_accountability_osp_codehigh <- pxi68[, 2]

# Prepare output object
out <- list()
out$cy <- acc
out$mcmc_posteriors <- hmodel
out$ITER <- ITER
out$model_code <- paste0(readLines(model_path), collapse = "\n")

output <- list()
output[[NAME]] <- out

write_file(
    output,
    file.path("out",  sprintf("%s_%s_%s", NAME, Sys.getenv("SLURM_JOB_ID"), ".rds"))
    )

info("Finished!")