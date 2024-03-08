library(vutils)
library(runjags)
library(rjags)
library(parallel)

sessionInfo()

NAME <- "v2x_accountability"
ITER <- 500L
BURNIN <- 5000L
THIN <- 20L
MCMC <- 2000L
WARMUP <- 5000L
CHAINS <- 8L


# Load Data
acc <- readRDS(file.path("input", "acc_input.rds"))
stopifnot(nrow(acc) != 0)
model_path <- file.path("jags", "accountability_MM.txt")
out_path <- file.path("out")

# Report some info
info("START: " %^% Sys.time())
info("NAME: " %^% NAME)
info("INFILE: " %^% file.path("input", "acc_input.rds"))
info("OUTDIR: " %^% out_path)
info("ITER: " %^% ITER)


###analysis
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

mon <- c("betaCS", "tauCS", "tauC", "betaMV", "tauMV", "tauM", "betaFE", "tauFE", "tauF",
  "betaES", "tauES",  "betaLC", "tauLC", "betaL", "betaJC", "tauJC", "tauJ", "betaIB", "tauIB",
  "betaEC", "tauEC","betaEV", "tauEV", "betaER", "betaPE", "tauPE", "betaP",
  "tauPP", "tauP", "tauSocial", "tauHorizontal", "tauVertical", "xi")

sprintf("%d runs with %d sampling iterations, %d burnin, and %d thin",
        ITER, MCMC, BURNIN, THIN) %>% info

sprintf("Found %d total obs", nrow(acc)) %>% info


hmodel <- run.jags(method = "parallel", model = model_path,
  monitor = mon, data = hmod, n.chains = CHAINS,
  inits = list(xi = 2 * acc$v2x_elecreg - 1),
  adapt = 5000, burnin = BURNIN, sample = ITER, thin = THIN,
  summarise = FALSE, plots = FALSE, modules = c("glm", "lecuyer"))

info("JAGS models finished")

###
cds <- as.mcmc.list(hmodel)

###check for convergence using gelman diagnostics
gelman.diag(cds[,1:111]) ###Parameters
blah <- gelman.diag(cds[, sample(112:(111 + nrow(acc)), MCMC)], multivariate = FALSE) ###Parameters
info("Proportion of R hat above 1.01: " %^% mean(blah[[1]][, 2] > 1.01))

###normalize posterior draws with this function

idbayes <- function(x){
  mx <- apply(x, 1, mean)
  sdx <- apply(x, 1, sd)
  idb <- matrix(nrow = nrow(x), ncol = ncol(x))
  for(j in 1:nrow(x)) {
    idb[j,] <- (x[j,] - mx[j]) / sdx[j]
  }
  return(idb)
}

cds <- combine.mcmc(hmodel$mcmc[, 112:(111 + nrow(acc))])
xi <- idbayes(cds)
mxi <- apply(xi, 2, median)
xi68 <- HPDinterval(as.mcmc(xi), prob = .68)
pxi <- pnorm(xi)
pmxi <- apply(pxi,2 , median)
pxi68 <- HPDinterval(as.mcmc(pxi), prob=.68)

acc$v2x_accountability <- mxi
acc$v2x_accountability_codelow <- xi68[, 1]
acc$v2x_accountability_codehigh <- xi68[, 2]
acc$v2x_accountability_osp <- pmxi
acc$v2x_accountability_osp_codelow <- pxi68[, 1]
acc$v2x_accountability_osp_codehigh <- pxi68[, 2]

out <- list()
out$cy <- acc
out$mcmc_posteriors <- hmodel
out$ITER <- ITER
out$model_code <- paste0(readLines(model_path), collapse = "\n")

output <- list()
output[[NAME]] <- out

write_file(output, file.path(out_path,
    NAME %^% "_" %^% Sys.getenv("SLURM_JOB_ID") %^% ".rds"))

info("Finished!")
