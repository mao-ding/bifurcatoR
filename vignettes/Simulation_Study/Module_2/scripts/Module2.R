devtools::install_github("VanAndelInstitute/bifurcatoR",force=T)

suppressPackageStartupMessages({
  library(bifurcatoR)
  library(doParallel)
  library(dplyr)
  library(data.table)
  library(parallel)
})

total_cores <- round(parallel::detectCores()*.95)

###################
## Normal & Weib ##
###################
params_grid <- rbind(expand.grid(
  Distribution = c("norm"),
  Alpha = 0.05,
  Sim = 1000,
  nboot = 1000,
  SampleSize = c(25,50,100,200,400),
  Prop = c(0.25, 0.5, 0.75),
  mu1 = 0,
  sd1 = 1,
  mu2 = c(0, 2, 4),
  sd2 = c(1, 2, 4)),
  expand.grid(
  Distribution = c("weib"),
  Alpha = 0.05,
  Sim = 1000,
  nboot = 1000,
  SampleSize = c(25,50,100,200,400),
  Prop = c(0.25, 0.5, 0.75),
  mu1 = 1,
  sd1 = 1,
  mu2 = c(1, 3, 5),
  sd2 = c(1, 2, 4))
)  

process_sims_w_n = function(row){  
  
  if(params_grid$Distribution[row] == "norm"){
    params_list <- list(p = params_grid$Prop[row],
                        mu1 = params_grid$mu1[row],
                        sd1 = params_grid$sd1[row],
                        mu2 = params_grid$mu2[row],
                        sd2 = params_grid$sd2[row])
    
  } else if(params_grid$Distribution[row] == "weib"){
    params_list <- list(p = params_grid$Prop[row],
                        sp1 = mixdist::weibullpar(params_grid$mu1[row],params_grid$sd1[row], loc = 0)$shape,
                        sc1 = mixdist::weibullpar(params_grid$mu1[row],params_grid$sd1[row], loc = 0)$scale,
                        sp2 = mixdist::weibullpar(params_grid$mu2[row],params_grid$sd2[row], loc = 0)$shape,
                        sc2 = mixdist::weibullpar(params_grid$mu2[row],params_grid$sd2[row], loc = 0)$scale)
  }
  
  output <- bifurcatoR::est_pow(
    n = params_grid$SampleSize[row],
    alpha = params_grid$Alpha[row],
    nsim = params_grid$Sim[row],
    dist = params_grid$Distribution[row],
    params = params_list,
    nboot = params_grid$nboot[row],
    tests = c("dip", "mclust", "mt","SI","HY","CH","ACR","FM","GmixR","WmixR")
  )
  return(cbind(params_grid[row, ], output))
}

start.time <- Sys.time()
results=rbindlist(mclapply(1:nrow(params_grid), function(x) process_sims_w_n(x),mc.cores=total_cores))


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


saveRDS(results, '~/Nadeau/module2_n_w_sim_1000.rds')

###################
#### Beta ####
###################
params_grid <- expand.grid(
  Distribution = c("beta"),
  Alpha = .05,
  Sim = 1000,
  nboot = 1000,
  SampleSize = c(25,50,100,200,400),
  #Prop = c(0.25, 0.5,.75),
  s1 = c(0.25,0.5),
  s2 = c(0.5,.75)
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
results=rbindlist(mclapply(1:nrow(params_grid), function(x) process_sims_b(x),mc.cores=total_cores))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# results
saveRDS(results, '~/Nadeau/module2_b_sim_1000.rds')


###################
## Lognormal Distribution ##
###################
params_grid <- expand.grid(
  Distribution = c("LNnorm"),
  Alpha = 0.05,
  Sim = 1000,
  nboot = 1000,
  SampleSize = c(50,100,200,400),
  Prop = c(0.25, 0.5, 0.75),
  mu1 = 0,
  sd1 = 1,
  mu2 = c(0, 2, 4),
  sd2 = c(1,  2, 4)
)  

#write.xlsx(params_grid, "~/scratch/array_result/LNormal/LN_params_grid.xlsx")

##### add this to store our slurm task variable
#args = commandArgs(trailingOnly=TRUE)

# print task number to keep track of task number
#print(paste0('Hello! I am task number: ', args[1]) )


process_sims_w_n = function(row){  
  
  if(params_grid$Distribution[row] == "norm"){
    params_list <- list(p = params_grid$Prop[row],
                        mu1 = params_grid$mu1[row],
                        sd1 = params_grid$sd1[row],
                        mu2 = params_grid$mu2[row],
                        sd2 = params_grid$sd2[row])
    
  } else if(params_grid$Distribution[row] == "weib"){
    params_list <- list(p = params_grid$Prop[row],
                        sp1 = mixdist::weibullpar(params_grid$mu1[row],params_grid$sd1[row], loc = 0)$shape,
                        sc1 = mixdist::weibullpar(params_grid$mu1[row],params_grid$sd1[row], loc = 0)$scale,
                        sp2 = mixdist::weibullpar(params_grid$mu2[row],params_grid$sd2[row], loc = 0)$shape,
                        sc2 = mixdist::weibullpar(params_grid$mu2[row],params_grid$sd2[row], loc = 0)$scale)
  }
  
  else if(params_grid$Distribution[row] == "LNnorm"){
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
results=rbindlist(mclapply(1:nrow(params_grid), function(x) process_sims_w_n(x),mc.cores=total_cores))


#results<-process_sims_w_n(as.integer(args[1]))

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# results
saveRDS(results, '~/module2_Lognormal_sim_1000.rds')