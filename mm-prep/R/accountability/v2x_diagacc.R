library(vutils)
library(rjags)
library(runjags)
library(parallel)


info("reading file...")

acc <- readRDS(file.path("input", "acc_input.rds"))

###analysis
hmod <- list(
  N = nrow(acc),
  b0 = rep(0, 2),
  B0 = diag(1, 2),
  yCS = cbind(acc$v2csreprss, acc$v2csprtcpt, acc$v2cseeorgs),
  yMV = cbind(acc$v2mebias, acc$v2mecrit,acc$v2merange, acc$v2mecenefm,
                 acc$v2meharjrn, acc$v2meslfcen),
  yFE = cbind(acc$v2cldiscm, acc$v2cldiscw, acc$v2clacfree, acc$v2mecenefi),
  yES = acc$v2dlengage)


hmodel <- run.jags(method = "parallel",
                   model = file.path("jags", "accountability_diagonal.txt"),
                   monitor = c("betaCS", "tauCS", "tauC",
                             "betaMV", "tauMV", "tauM",
                             "betaFE", "tauFE", "tauF",
                             "betaES", "tauES", "xiS",
                             "xiC", "xiF", "xiM"),
                   data = hmod, n.chains = 32,
                   inits = list(xiS = 2 * as.numeric(acc$v2dlengage > median(acc$v2dlengage, na.rm=T)) - 1),
                   adapt = 5000, burnin = 2500, sample = 250, thin = 10,
                   summarise = FALSE, plots = FALSE, modules = c("glm", "lecuyer"))


###check for convergence using gelman diagnostics
###
library(runjags)
cds <- as.mcmc.list(hmodel)

###check for convergence using gelman diagnostics
gelman.diag(cds[,1:45]) ###Parameters
blah <- gelman.diag(cds[,sample(46:(45+nrow(acc)),2000)],multivariate=F) ###Parameters
mean(blah[[1]][,2]>1.01)
blah <- gelman.diag(cds[,sample((46+nrow(acc)):(45+2*nrow(acc)),2000)],multivariate=F) ###Parameters
mean(blah[[1]][,2]>1.01)
blah <- gelman.diag(cds[,sample((46+2*nrow(acc)):(45+3*nrow(acc)),2000)],multivariate=F) ###Parameters
mean(blah[[1]][,2]>1.01)
blah <- gelman.diag(cds[,sample((46+3*nrow(acc)):(45+4*nrow(acc)),2000)],multivariate=F) ###Parameters
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

cds <- combine.mcmc(hmodel$mcmc[,46:(45+nrow(acc))])
xi <- idbayes(cds)
mxi <- apply(xi,2,median)
xi68 <- HPDinterval(as.mcmc(xi),prob=.68)
pxi <- pnorm(xi)
pmxi <- apply(pxi,2,median)
pxi68 <- HPDinterval(as.mcmc(pxi),prob=.68)

acc$v2x_diagacc <- mxi
acc$v2x_diagacc_codelow <- xi68[,1]
acc$v2x_diagacc_codehigh <- xi68[,2]
acc$v2x_diagacc_osp <- pmxi
acc$v2x_diagacc_osp_codelow <- pxi68[,1]
acc$v2x_diagacc_osp_codehigh <- pxi68[,2]

out <- list()
out[["v2x_diagacc"]]$cy <- acc
out[["v2x_diagacc"]]$hmodel <- hmodel
write_file(out, file.path("out", "v2x_diagacc" %^% "_" %^%
        Sys.getenv("SLURM_JOB_ID") %^% ".rds"))
print("Finished calculation :)")
