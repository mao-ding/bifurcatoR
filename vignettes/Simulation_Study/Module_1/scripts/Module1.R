
devtools::install_github("VanAndelInstitute/bifurcatoR",force=T)

# If errors happen, install package from Zach's branch
devtools::install_github("madajz/bifurcatoR",force=T)

suppressPackageStartupMessages({
  library(bifurcatoR)
  library(doParallel)
  library(dplyr)
  library(mixdist)
  library(parallel)
  library(data.table)
  library(Hmisc)
  library(matrixStats)
})


total_cores <- round(parallel::detectCores()*.95)

params_grid <- rbind(expand.grid(
  Distribution = c("norm"),
  Alpha = 0.05,
  Sim = 1000,
  SampleSize = c(20, 50, 100, 200, 400),
  Prop = c(0.25, 0.5, 0.75),
  Modes = 1,
  mu1 = 0,
  sd1 = 1,
  mu2 = c(0, 2, 4),
  sd2 = c(1, 2, 4),
  Perm = c(1000)),
  expand.grid(
    Distribution = c("weib"),
    Alpha = 0.05,
    Sim = 1000,
    SampleSize = c(20, 50, 100, 200, 400),
    Prop = c(0.25, 0.5, 0.75),
    Modes = 1,
    mu1 = 1,
    sd1 = 1,
    mu2 = c(1,3,5),
    sd2 = c(1,2,4),
    Perm = c(1000))
)

process_sims_w_n = function(row){
  params_list <- list(mean = params_grid$mu2[row],
                      v_scale = params_grid$sd2[row])
  
  n2 <- floor(params_grid$Prop[row] * params_grid$SampleSize[row])
  n1 <- params_grid$SampleSize[row] - n2
  
  output <- bifurcatoR::est_pow_2samp(
    n1 = n1,
    n2 = n2,
    alpha = params_grid$Alpha[row],
    nsim = params_grid$Sim[row],
    modes = params_grid$Modes[row],
    dist = params_grid$Distribution[row],
    params = params_list,

    tests = c("Levene","Permutations (Gini)", "Permutations (MAD)", "Permutations (SD)","ANOVA", "Non-parametric ANOVA","Permutations (Raw)"),
    
    nperm = params_grid$Perm[row]
  )
  
  return(cbind(params_grid[row, ], output))
}

start.time <- Sys.time()
results=rbindlist(mclapply(1:nrow(params_grid), function(x) process_sims_w_n(x),mc.cores=total_cores))
end.time <- Sys.time()
time.taken <- end.time - start.time

time.taken

# results
saveRDS(results, '~/module1_n_w_7_tests_sim(1000).rds')


