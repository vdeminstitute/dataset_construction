# Script to run the diagonal accountability index.

library(vutils)
library(rjags)
library(runjags)
library(parallel)

sessionInfo()

NAME <- "v2x_diagacc"
ITER <- 250L
BURNIN <- 2500L
THIN <- 10L
MCMC <- 2000L # Used to determine how many samples to draw from the posterior result
WARMUP <- 5000L
CHAINS <- 32L

# Load input file
acc <- readRDS(file.path("input", "acc_input.rds"))
# Load model script
model_path <- file.path("jags", "accountability_diagonal.txt")

info(sprintf("%d runs with %d sampling iterations, %d burnin, and %d thin",
        ITER, MCMC, BURNIN, THIN))

info(sprintf("Found %d total obs", nrow(acc)))

# Transform input data to JAGS format
hmod <- list(
    N = nrow(acc),
    b0 = rep(0, 2),
    B0 = diag(1, 2),
    yCS = cbind(acc$v2csreprss, acc$v2csprtcpt, acc$v2cseeorgs),
    yMV = cbind(acc$v2mebias, acc$v2mecrit,acc$v2merange, acc$v2mecenefm,
                    acc$v2meharjrn, acc$v2meslfcen),
    yFE = cbind(acc$v2cldiscm, acc$v2cldiscw, acc$v2clacfree, acc$v2mecenefi),
    yES = acc$v2dlengage
)

# Estimate posteriors
hmodel <- run.jags(method = "parallel",
    model = model_path,
    monitor = c("betaCS", "tauCS", "tauC", "betaMV", "tauMV", "tauM",
    "betaFE", "tauFE", "tauF", "betaES", "tauES", "xiS", "xiC", 
    "xiF", "xiM"),
    data = hmod, 
    n.chains = CHAINS,
    inits = list(xiS = 2 * as.numeric(acc$v2dlengage > median(acc$v2dlengage, na.rm=T)) - 1),
    adapt = WARMUP, 
    burnin = BURNIN, 
    sample = ITER, 
    thin = THIN,
    summarise = FALSE, 
    plots = FALSE, 
    modules = c("glm", "lecuyer")
)

info("JAGS models finished")
posteriorDraws <- as.mcmc.list(hmodel)

# Check for convergence using Gelman diagnostics
gelman.diag(posteriorDraws[,1:45]) # All parameters except xiS, xiC, xiF, xiM

xiSRhat <- gelman.diag(posteriorDraws[,sample(46:(45+nrow(acc)), MCMC)],multivariate=F) ###Parameters
info("Proportion of R hat above 1.01: " %^% mean(xiSRhat[[1]][,2]>1.01))

xiCRhat <- gelman.diag(posteriorDraws[,sample((46+nrow(acc)):(45+2*nrow(acc)), MCMC)],multivariate=F) ###Parameters
mean(xiCRhat[[1]][,2]>1.01)

xiFRhat <- gelman.diag(posteriorDraws[,sample((46+2*nrow(acc)):(45+3*nrow(acc)), MCMC)],multivariate=F) ###Parameters
mean(xiFRhat[[1]][,2]>1.01)

xiMRhat <- gelman.diag(posteriorDraws[,sample((46+3*nrow(acc)):(45+4*nrow(acc)), MCMC)],multivariate=F) ###Parameters
mean(xiMRhat[[1]][,2]>1.01)

# Standardize the posterior draws to have mean 0 and variance 1
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
# -- Note, we use xiS as xi
xi <- idbayes(combine.mcmc(hmodel$mcmc[,46:(45+nrow(acc))]))

# Standard model estimates of index
mxi <- apply(xi,2,median) # point-estimate for index
xi68 <- HPDinterval(as.mcmc(xi),prob=.68) # credible interval for index

# Original scale predictions of index
pxi <- pnorm(xi) # Make xi vary from 0 to 1
pmxi <- apply(pxi,2,median) # median
pxi68 <- HPDinterval(as.mcmc(pxi),prob=.68) # credible interval

# Store estimates in input object
acc$v2x_diagacc <- mxi
acc$v2x_diagacc_codelow <- xi68[,1]
acc$v2x_diagacc_codehigh <- xi68[,2]
acc$v2x_diagacc_osp <- pmxi
acc$v2x_diagacc_osp_codelow <- pxi68[,1]
acc$v2x_diagacc_osp_codehigh <- pxi68[,2]

# Prepare output object
out <- list()
out[[NAME]]$cy <- acc
out[[NAME]]$mcmc_posteriors <- hmodel
out[[NAME]]$ITER <- ITER
out[[NAME]]$model_code <- paste0(readLines(model_path), collapse = "\n")

write_file(out, file.path("out", NAME %^% "_" %^% Sys.getenv("SLURM_JOB_ID") %^% ".rds"))

info("Finished!")
