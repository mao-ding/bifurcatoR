#change the working directory to reflect where you want the data results to go
setwd("/work/larylab/Mao_Ding/bifurcatoR/vignettes/Simulation_Study/Module_2/results/raw_results")

suppressPackageStartupMessages({
  library(bifurcatoR)
  library(doParallel)
  library(dplyr)
  library(data.table)
  library(parallel)
})

total_cores = round(parallel::detectCores() * 0.95)

##########################
#### Beta Distribution ####
##########################
params_grid <- expand.grid(
  Distribution = c("beta"),
  Alpha = 0.05,
  Sim = 1000,
  nboot = 1000,
  SampleSize = c(25,50,100,200,400),
  s1 = c(0.25, 0.5),
  s2 = c(0.5, 0.75)
)


process_sims_b = function(row){  
  params_list <- list(s1 = params_grid$s1[row],
                      s2 = params_grid$s2[row])
  
  output <- bifurcatoR::est_pow(
    n = params_grid$SampleSize[row],
    alpha = params_grid$Alpha[row],
    nsim = params_grid$Sim[row],
    dist = params_grid$Distribution[row],
    params = params_list,
    nboot = params_grid$nboot[row],
    tests = c("dip", "mclust", "mt","SI","HY","CH","ACR","FM")
  )
  
  return(cbind(params_grid[row, ], output))
}

start.time <- Sys.time()

results = process_sims_b(as.integer(args[1]))

time.taken <- end.time - start.time
time.taken


saveRDS(results, paste('module_2_b_results', args, sep="_"),)




