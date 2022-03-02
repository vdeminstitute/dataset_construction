library(vutils)
library(rjags)
library(runjags)
library(parallel)

sessionInfo()

NAME <- "v2x_horacc"
ITER <- 250L
BURNIN <- 10000L
THIN <- 40L
MCMC <- 2000L
WARMUP <- 1000L
CHAINS <- 8L

acc <- readRDS(file.path("input", "acc_input.rds"))
model_path <- file.path("jags", "accountability_horizontal.txt")

sprintf("%d runs with %d sampling iterations, %d burnin, and %d thin",
        ITER, MCMC, BURNIN, THIN) %>% info

sprintf("Found %d total obs", nrow(acc)) %>% info


###analysis
hmod <- list(
  N = nrow(acc),
  b0 = rep(0, 2),
  B0 = diag(1, 2),
  yLC = cbind(acc$v2lginvstp, acc$v2lgqstexp),
  yL = acc$v2lgbicam,
  yJC = cbind(acc$v2jucomp, acc$v2juhccomp, acc$v2juhcind, acc$v2juncind),
  yIB = acc$v2lgotovst,
  yEC = acc$v2exrescon)

hmodel <- run.jags(method = "parallel", model = model_path,
  monitor = c("betaLC", "tauLC", "betaL", "betaJC", "tauJC","tauJ", "betaIB", "tauIB", "betaEC",
    "tauEC", "xiH", "xiJ"),
  data = hmod, n.chains = CHAINS,
  inits = list(xiH = 2 * as.numeric(acc$v2exrescon > median(acc$v2exrescon, na.rm = TRUE)) - 1),
  adapt = WARMUP, burnin = BURNIN, sample = ITER, thin = THIN,
  summarise = FALSE, plots = FALSE, modules = c("glm", "lecuyer"))

info("JAGS models finished")



cds <- as.mcmc.list(hmodel)
###check for convergence using gelman diagnostics
gelman.diag(cds[,1:27]) ###Parameters
blah <- gelman.diag(cds[,sample(28:(27+nrow(acc)), MCMC)],multivariate=F) ###xiH
info("Proportion of R hat above 1.01: " %^% mean(blah[[1]][,2]>1.01))
blah <- gelman.diag(cds[,sample((28+nrow(acc)):(27+2*nrow(acc)), MCMC)],multivariate=F)###xiJ
mean(blah[[1]][,2]>1.01)


###xiH = horizontal accountability

###normalize posterior draws with this function

idbayes <- function(x){
  mx <- apply(x,1,mean)
  sdx <- apply(x,1,sd)
  idb <- matrix(nrow=nrow(x), ncol=ncol(x))
  for(j in 1:nrow(x)){
    idb[j,] <-  (x[j,]-mx[j])/sdx[j]
  }
  return(idb)
}


cds <- combine.mcmc(hmodel$mcmc[,28:(27+nrow(acc))])
xi <- idbayes(cds)
mxi <- apply(xi,2,median)
xi68 <- HPDinterval(as.mcmc(xi),prob=.68)
pxi <- pnorm(xi)
pmxi <- apply(pxi,2,median)
pxi68 <- HPDinterval(as.mcmc(pxi),prob=.68)

acc$v2x_horacc <- mxi
acc$v2x_horacc_codelow <- xi68[,1]
acc$v2x_horacc_codehigh <- xi68[,2]
acc$v2x_horacc_osp <- pmxi
acc$v2x_horacc_osp_codelow <- pxi68[,1]
acc$v2x_horacc_osp_codehigh <- pxi68[,2]

out <- list()
out[[NAME]]$cy <- acc
out[[NAME]]$mcmc_posteriors <- hmodel
out[[NAME]]$ITER <- ITER
out[[NAME]]$model_code <- paste0(readLines(model_path), collapse = "\n")

write_file(out, file.path("out", NAME %^% "_" %^%
        Sys.getenv("SLURM_JOB_ID") %^% ".rds"))

info("Finished!")
