#devtools::install_github("VanAndelInstitute/bifurcatoR",force=T)

setwd("/work/larylab/Mao_Ding/bifurcatoR/vignettes/dxsmall/results/raw_results")

suppressPackageStartupMessages({
  library(bifurcatoR)
  library(doParallel)
  library(dplyr)
  library(data.table)
  library(parallel)
  library(mclust)
  library(Rcpp)
})


help("dxsmall")
data(dxsmall)


##### Fit the data use Mclust #######
fit_1 = Mclust(logcounts(dxsmall)["MECOM", dxsmall$FusionGroup == "MLL"], G = 2)
fit_2 = Mclust(logcounts(dxsmall)["MECOM", dxsmall$FusionGroup == "NSD1"], G = 2)

total_cores <- round(parallel::detectCores()*.95)

##### Create parameters grid #####
params_grid_1 <- rbind(expand.grid(
  Distribution = c("norm"),
  Fusion_Group = 'MLL',
  Gene = 'MECOM',
  Alpha = .05,
  Sim = 1000,
  nboot = 1000,
  SampleSize = c(470,300,200,100,50),
  #SampleSize = 50,
  Prop = (table(fit_1$classification)/sum(dxsmall$FusionGroup == "MLL"))[1],
  mu1 = fit_1$parameters$mean[1],
  sd1 = (sqrt(fit_1$parameters$variance$sigmasq))[1],
  mu2 = fit_1$parameters$mean[2],
  sd2 = (sqrt(fit_1$parameters$variance$sigmasq))[2]
))

params_grid_2 <- rbind(expand.grid(
  Distribution = c("norm"),
  Fusion_Group = 'NSD1',
  Gene = 'MECOM',
  Alpha = .05,
  Sim = 1000,
  nboot = 1000,
  SampleSize = c(470,300,200,100,50),
  #SampleSize = 50,
  Prop = (table(fit_2$classification)/sum(dxsmall$FusionGroup == "NSD1"))[1],
  mu1 = fit_2$parameters$mean[1],
  sd1 = (sqrt(fit_2$parameters$variance$sigmasq))[1],
  mu2 = fit_2$parameters$mean[2],
  sd2 = (sqrt(fit_2$parameters$variance$sigmasq))[1] # only one sd for fit_2, so set sd2 as same as sd1
))

params_grid = rbind(params_grid_1, params_grid_2)

##### add this to store our slurm task variable
args = commandArgs(trailingOnly=TRUE)

# print task number to keep track of task number
print(paste0('Hello! I am task number: ', args[1]) )

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
    tests = c("dip", "mclust", "mt", "SI","HY","CH","ACR","FM","GmixR")
  )

  return(cbind(params_grid[row, ], output))
}

start.time <- Sys.time()

results = process_sims_n_w(as.integer(args[1]))
#results = rbindlist(mclapply(1:nrow(params_grid), function(x) process_sims_w_n(x),mc.cores=total_cores))

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

saveRDS(results, paste('dxsmall_simulation_results', args, sep="_"),)
