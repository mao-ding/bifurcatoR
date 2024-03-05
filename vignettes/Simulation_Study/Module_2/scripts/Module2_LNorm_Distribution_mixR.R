#change the working directory to reflect where you want the data results to go
setwd("/work/larylab/Mao_Ding/bifurcatoR/vignettes/Simulation_Study/Module_2/results/raw_results")

suppressPackageStartupMessages({
  library(bifurcatoR)
  library(doParallel)
  library(dplyr)
  library(data.table)
  library(parallel)
  library(mixR)
})

###################
## Lognormal Distribution ##
###################
params_grid <- expand.grid(
  Distribution = c("LNnorm"),
  Alpha = .05,
  Sim = 1000,
  nboot = 1000,
  SampleSize = c(50,100,200,400),
  Prop = c(0.25, 0.5, 0.75),
  mu1 = 0,
  sd1 = 1,
  mu2 = c(0, 2, 4),
  sd2 = c(1, 2, 4)
)  

##### add this to store our slurm task variable
args = commandArgs(trailingOnly=TRUE)

# print task number to keep track of task number
print(paste0('Hello! I am task number: ', args[1]))


process_sims_ln = function(row){  
  
  if(params_grid$Distribution[row] == "LNnorm"){
    params_list = list(p = params_grid$Prop[row],
                       mu1 = params_grid$mu1[row],
                       sd1 = params_grid$sd1[row],
                       mu2 = params_grid$mu2[row],
                       sd2 = params_grid$sd2[row])
  }
  
  output <- bifurcatoR::est_pow(
    n = params_grid$SampleSize[row],
    alpha = params_grid$Alpha[row],
    nsim = params_grid$Sim[row],
    dist = params_grid$Distribution[row],
    params = params_list,
    nboot = params_grid$nboot[row],
    tests = c("LNmixR")
  )
  return(cbind(params_grid[row, ], output))
}

start.time <- Sys.time()

results = process_sims_ln(as.integer(args[1]))

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

saveRDS(results, paste('module_2_ln_mixR_results', args, sep="_"),)