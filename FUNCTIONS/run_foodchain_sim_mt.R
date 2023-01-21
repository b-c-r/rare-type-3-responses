################################################################################
#    Function "run_foodchain_sim_mt" to run the 3-species food chain across    #
#    a gradient of q values in  parallel                                       #
#                                                                              #
#    Copyright (C) 2023 Björn C. Rall                                          #
#                                                                              #
#    This program is free software: you can redistribute it and/or modify      #
#    it under the terms of the GNU General Public License as published by      #
#    the Free Software Foundation, either version 3 of the License, or         #
#    (at your option) any later version.                                       #
#                                                                              #
#    This program is distributed in the hope that it will be useful,           #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#    GNU General Public License for more details.                              #
#                                                                              #
#    You should have received a copy of the GNU General Public License         #
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.    #
################################################################################

# Description:
# The function simulates the 3-species food chain across a gradient of q values
# and saves the output as *.csv files for a bifurcation diagram. The food-chain
# model is based on Otto et al. (2007) with adaptions from Williams and Martinez
# (2004). See also the supplement information of Kalinkat et al. (under review)
# for details. The package odeintr (Keitt 2007) is required to run the function.
# This version of the function is a wrapper around run_foodchain_sim() to run
# the code in parallel.
#
# Arguments:
# qrange:      A vector with all q-values to be simulated (default = seq(0, .2, length = 501)
# ts_length:   The length of each time series to be simulated (default = 100000)
# steplength:  The explicit times steps that will be simulated (default = .5)
# analyze_ts:  The percentage of the time series that should be analyzed (e.g.
#              The last 5 %) (default = 0.05; between 0 and 1)
# unique_out:  Should double entries within one q value be deleted? (default = T),
# max_out:     Number of different extrema to be saved (default = 20; 0 == All are save),
# output_path: the (sub-)path were the output files should be saved to (default = "SIM_OUT/")
# noC:         the number of threads that should be used for parallel computing (default to all possible threads)
# 
# Requirements:
# Compilation of the food chain model (run compile_foodchain())
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr)
# Needs the package parallel
# Needs the package foreach (https://CRAN.R-project.org/package=foreach)
# Needs the package iterators (https://CRAN.R-project.org/package=iterators)
# Needs the package doParallel (https://CRAN.R-project.org/package=doParallel)
# 
# 
# References
# Kalinkat et al. (under review): Empirical evidence of type III functional responses and why it remains rare.
# Keitt (2007): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71, https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359.
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1.
#

run_foodchain_sim_mt <- function(qrange = seq(0, .2, length = 501),
                                 ts_length = 100000,
                                 steplength = .5,
                                 analyze_ts = 0.05,
                                 unique_out = T,
                                 max_out = 20,
                                 output_path = "SIM_OUT/",
                                 noC = ceiling(parallel::detectCores()*0.75)){

  if(!dir.exists(output_path)){
    dir.create(output_path)
    warning("The output path does not exist - I'll create it for you.")
  }
  
  qrange <- sample(qrange)
  splits_q <- rep(floor(length(qrange)/noC), noC-1)
  splits_q <- cumsum(splits_q)
  splits_q <- c(0, splits_q, (length(qrange)-max(splits_q))+max(splits_q))
  
  qranges <- foreach::foreach(i = 1:noC) %do% {
    qrange[(splits_q[i]+1):splits_q[i+1]]
  }
  
  cl <- parallel::makeCluster(noC)
  doParallel::registerDoParallel(cl)
  foreach::foreach(i = 1:noC,
                   .packages = "odeintr",
                   .export = c("compile_foodchain",
                               "run_foodchain_sim",
                               "set_foodchain_parms",
                               "minmax")) %dopar% {
    compile_foodchain()
                     
    ## run the simulation
    run_foodchain_sim(qrange = qranges[[i]],
                      ts_length = ts_length,
                      steplength = steplength,
                      analyze_ts = analyze_ts,
                      unique_out = unique_out,
                      max_out = max_out,
                      output_path = paste(output_path,i,sep=""),
                      create.folder = F)
    
  }
  parallel::stopCluster(cl)
  
  Basal <- read.csv(paste(output_path,1,"bifout_basal.csv",sep=""))
  unlink(paste(output_path,1,"bifout_basal.csv",sep=""))
  
  Inter <- read.csv(paste(output_path,1,"bifout_intermediate.csv",sep=""))
  unlink(paste(output_path,1,"bifout_intermediate.csv",sep=""))
  
  Top <- read.csv(paste(output_path,1,"bifout_top.csv",sep=""))
  unlink(paste(output_path,1,"bifout_top.csv",sep=""))
  
  for(i in 2:noC){
    Basal <- rbind(Basal, read.csv(paste(output_path,i,"bifout_basal.csv",sep="")))
    unlink(paste(output_path,i,"bifout_basal.csv",sep=""))
    
    Inter <- rbind(Inter, read.csv(paste(output_path,i,"bifout_intermediate.csv",sep="")))
    unlink(paste(output_path,i,"bifout_intermediate.csv",sep=""))
    
    Top <- rbind(Top, read.csv(paste(output_path,i,"bifout_top.csv",sep="")))
    unlink(paste(output_path,i,"bifout_top.csv",sep=""))
  }
  
  ## save results to hard-drive
  write.csv(Basal,
    paste(output_path, "bifout_basal.csv", sep =""),
    row.names = F
  )
  
  write.csv(Inter,
    paste(output_path, "bifout_intermediate.csv", sep =""),
    row.names = F
  )
  
  write.csv(Top,
    paste(output_path, "bifout_top.csv", sep =""),
    row.names = F
  )
}



