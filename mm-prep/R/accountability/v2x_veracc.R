library(vutils)
library(rjags)
library(runjags)
library(parallel)

sessionInfo()

NAME <- "v2x_veracc"
ITER <- 125L
BURNIN <- 15000L
THIN <- 120L
MCMC <- 2000L
WARMUP <- 5000L
CHAINS <- 32L

acc <- readRDS(file.path("input", "acc_input.rds"))
model_path <- file.path("jags", "accountability_vertical.txt")

sprintf("%d runs with %d sampling iterations, %d burnin, and %d thin",
        ITER, MCMC, BURNIN, THIN) %>% info

sprintf("Found %d total obs", nrow(acc)) %>% info


###analysis
hmod <- list(
  N = nrow(acc),
  b0 = rep(0, 2),
  B0 = diag(1, 2),
  yEV = cbind(acc$v2elembaut, acc$v2elembcap,	acc$v2elrgstry, acc$v2elirreg, acc$v2elintim,
    acc$v2elfrfair, acc$v2elmulpar),
  yER = acc$v2x_elecreg,
  yPE = acc$v2x_suffr,
  yO3 = acc$HoEel,
  yP = cbind(acc$v2psparban, acc$v2psbars, acc$v2psoppaut))


hmodel <- run.jags(method = "parallel",
  model = model_path,
  monitor = c("betaEV", "tauEV", "betaER", "betaPE", "tauPE","betaO3", "betaP", "tauPP", "tauP",
    "xiV", "xiP"),
  data = hmod, n.chains = CHAINS,  adapt = WARMUP, inits = list(xiV = 2 * acc$v2x_elecreg - 1),
  burnin = BURNIN, sample = ITER, thin = THIN, summarise = FALSE, plots = FALSE,
  modules = c("glm", "lecuyer"))

info("JAGS models finished")

cds <- as.mcmc.list(hmodel)

###check for convergence using gelman diagnostics
gelman.diag(cds[,c(1:7,15:21,24:38)],multivariate=F) ###Most Parameters


####Create mcmc list with transformed parameters for election variables (i.e. v2el... discrimination parameters x v2x_elecreg difficulty and discrimination)
cds_x <- list("1"=matrix(nrow=nrow(cds[[1]]),ncol=7),"2"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "3"=matrix(nrow=nrow(cds[[1]]),ncol=7),"4"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "5"=matrix(nrow=nrow(cds[[1]]),ncol=7),"6"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "7"=matrix(nrow=nrow(cds[[1]]),ncol=7),"8"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "9"=matrix(nrow=nrow(cds[[1]]),ncol=7),"10"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "11"=matrix(nrow=nrow(cds[[1]]),ncol=7),"12"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "13"=matrix(nrow=nrow(cds[[1]]),ncol=7),"14"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "15"=matrix(nrow=nrow(cds[[1]]),ncol=7),"16"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "17"=matrix(nrow=nrow(cds[[1]]),ncol=7),"18"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "19"=matrix(nrow=nrow(cds[[1]]),ncol=7),"20"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "21"=matrix(nrow=nrow(cds[[1]]),ncol=7),"22"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "23"=matrix(nrow=nrow(cds[[1]]),ncol=7),"24"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "25"=matrix(nrow=nrow(cds[[1]]),ncol=7),"26"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "27"=matrix(nrow=nrow(cds[[1]]),ncol=7),"28"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "29"=matrix(nrow=nrow(cds[[1]]),ncol=7),"30"=matrix(nrow=nrow(cds[[1]]),ncol=7),
            "31"=matrix(nrow=nrow(cds[[1]]),ncol=7),"32"=matrix(nrow=nrow(cds[[1]]),ncol=7))

for(i in 1:32){
  for(j in 8:14){
  cds_x[[i]][,j-7] <- cds[[i]][,j]*(cds[[i]][,22]+cds[[i]][,23])
  cds_x[[i]] <- as.mcmc(cds_x[[i]])
  }
}


cds_x <- as.mcmc.list(cds_x)
gelman.diag(cds_x,multivariate=F) ###Check convergence of election parameters


blah <- gelman.diag(cds[,sample(39:(38+nrow(acc)), MCMC)],multivariate=F) ###Parameters
info("Proportion of R hat above 1.01:" %^% mean(blah[[1]][,2]>1.01))
blah <- gelman.diag(cds[,sample((39+nrow(acc)):(38+2*nrow(acc)), MCMC)],multivariate=F) ###Parameters
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

cds <- combine.mcmc(hmodel$mcmc[, 39:(38 + nrow(acc))])
xi <- idbayes(cds)
mxi <- apply(xi, 2, median)
xi68 <- HPDinterval(as.mcmc(xi), prob = .68)
pxi <- pnorm(xi)
pmxi <- apply(pxi, 2, median)
pxi68 <- HPDinterval(as.mcmc(pxi), prob = .68)

acc$v2x_veracc <- mxi
acc$v2x_veracc_codelow <- xi68[,1]
acc$v2x_veracc_codehigh <- xi68[,2]
acc$v2x_veracc_osp <- pmxi
acc$v2x_veracc_osp_codelow <- pxi68[,1]
acc$v2x_veracc_osp_codehigh <- pxi68[,2]

out <- list()
out[[NAME]]$cy <- acc
out[[NAME]]$mcmc_posteriors <- hmodel
out[[NAME]]$ITER <- ITER
out[[NAME]]$model_code <- paste0(readLines(model_path), collapse = "\n")

write_file(out, file.path("out", NAME %^% "_" %^%
        Sys.getenv("SLURM_JOB_ID") %^% ".rds"))

info("Finished!")
