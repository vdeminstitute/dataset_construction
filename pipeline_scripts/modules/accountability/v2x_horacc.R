# Script to estimate horizontal accountability index. 
library(vutils)
library(rjags)
library(runjags)
library(parallel)

sessionInfo()

NAME <- "v2x_horacc"
ITER <- 250L
BURNIN <- 10000L
THIN <- 40L
MCMC <- 2000L # Used to determine how many samples to draw from the posterior result
WARMUP <- 1000L
CHAINS <- 8L

# Load input file
acc <- readRDS(file.path("input", "acc_input.rds"))
# Load model script
model_path <- file.path("jags", "accountability_horizontal.txt")

info(sprintf("%d runs with %d sampling iterations, %d burnin, and %d thin",
        ITER, MCMC, BURNIN, THIN))

info(sprintf("Found %d total obs", nrow(acc)))

# Transform input data to JAGS format
hmod <- list(
  N = nrow(acc),
  b0 = rep(0, 2),
  B0 = diag(1, 2),
  yLC = cbind(acc$v2lginvstp, acc$v2lgqstexp),
  yL = acc$v2lgbicam,
  yJC = cbind(acc$v2jucomp, acc$v2juhccomp, acc$v2juhcind, acc$v2juncind),
  yIB = acc$v2lgotovst,
  yEC = acc$v2exrescon)

# Estimate posteriors
hmodel <- run.jags(
  method = "parallel",
  model = model_path,
  monitor = c("betaLC", "tauLC", "betaL", "betaJC", "tauJC","tauJ", "betaIB", "tauIB", "betaEC",
    "tauEC", "xiH", "xiJ"),
  data = hmod,
  n.chains = CHAINS,
  inits = list(xiH = 2 * as.numeric(acc$v2exrescon > median(acc$v2exrescon, na.rm = TRUE)) - 1),
  adapt = WARMUP,
  burnin = BURNIN,
  sample = ITER,
  thin = THIN,
  summarise = FALSE,
  plots = FALSE,
  modules = c("glm", "lecuyer"))

info("JAGS models finished")
posteriorDraws <- as.mcmc.list(hmodel)

# Check for convergence using Gelman diagnostics
gelman.diag(posteriorDraws[,1:27]) # All parameters except xiH and xiJ

xiHRhat <- gelman.diag(posteriorDraws[,sample(28:(27+nrow(acc)), MCMC)],multivariate=F) ###xiH
info("Proportion of R hat above 1.01: " %^% mean(xiHRhat[[1]][,2]>1.01))

xiJRhat <- gelman.diag(posteriorDraws[,sample((28+nrow(acc)):(27+2*nrow(acc)), MCMC)],multivariate=F)###xiJ
mean(xiJRhat[[1]][,2]>1.01)

# Standardize posterior draws to have mean 0 and variance 1
idbayes <- function(x){
  mx <- apply(x,1,mean)
  sdx <- apply(x,1,sd)
  idb <- matrix(nrow=nrow(x), ncol=ncol(x))
  for(j in 1:nrow(x)){
    idb[j,] <-  (x[j,]-mx[j])/sdx[j]
  }
  return(idb)
}

# Extract country-date posterior samples
# -- Note, we use xiH as xi
xi <- idbayes(combine.mcmc(hmodel$mcmc[, 28:(27+nrow(acc))]))

# Standard model estimates of index 
mxi <- apply(xi, 2, median) # point-estimate for index
xi68 <- HPDinterval(as.mcmc(xi), prob = .68) # credible interval for index

# Original scale predictions of index
pxi <- pnorm(xi) # Make xi vary from 0 to 1
pmxi <- apply(pxi, 2, median) # median
pxi68 <- HPDinterval(as.mcmc(pxi), prob= .68) # credible interval

# Store estimates in input object
acc$v2x_horacc <- mxi
acc$v2x_horacc_codelow <- xi68[,1]
acc$v2x_horacc_codehigh <- xi68[,2]
acc$v2x_horacc_osp <- pmxi
acc$v2x_horacc_osp_codelow <- pxi68[,1]
acc$v2x_horacc_osp_codehigh <- pxi68[,2]

# Prepare output object
out <- list()
out[[NAME]]$cy <- acc
out[[NAME]]$mcmc_posteriors <- hmodel
out[[NAME]]$ITER <- ITER
out[[NAME]]$model_code <- paste0(readLines(model_path), collapse = "\n")

write_file(out, file.path("out", NAME %^% "_" %^% Sys.getenv("SLURM_JOB_ID") %^% ".rds"))

info("Finished!")
