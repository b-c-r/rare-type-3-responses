################################################################################
#    Function "run_foodchain_sim" to run the 3-species food chain across       #
#    a gradient of q values                                                    #
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
#
# Arguments:
# qrange:      a vector with all q-values to be simulated (default = seq(0, .2, length = 2000)
# ts_length:   the length of each time series to be simulated (default = 200000)
# steplength:  the explicit times steps that will be simulated (default = .5)
# analyze_ts:  the percentage of the time series that should be analyzed (e.g.
#              the last 5 %) (default = 0.05; between 0 and 1)
# output_path: the (sub-)path were the output files should be saved to (default = "SIM_OUT/")
# 
# The default settings are as used in Kalinkat et al. (under review).
# 
# Requirements:
# Compilation of the food chain model (run compile_foodchain())
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr)
# 
# References
# Kalinkat et al. (under review): Empirical evidence of type III functional responses and why it remains rare.
# Keitt (2007): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71, https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359.
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1.
#

run_foodchain_sim <- function(qrange = seq(0, .2, length = 2000),
                              ts_length = 200000,
                              steplength = .5,
                              analyze_ts = 0.05,
                              output_path = "SIM_OUT/"){
  
  ## setup empty vectors to collect bifurcation output
  Bx <- c()                                                                     # q-values for basal species
  By <- c()                                                                     # extrema for basal species
  Ix <- c()                                                                     # q-values for intermediate species
  Iy <- c()                                                                     # extrema for intermediate species
  Tx <- c()                                                                     # q-values for top species
  Ty <- c()                                                                     # extrema for top species
  
  for(i in 1:length(qrange)){
    
    ## setup the model parameter values
    set_foodchain_parms(q = qrange[i])
    
    ## run the simulation
    out <- chainC_adap(c(runif(1, .1, 1),
                         runif(1, .1, 1),
                         runif(1, .1, 1)),
                       ts_length,
                       steplength)
    
    ## cut the time series for further analyses
    out <- out[out$Time > max(out$Time)*(1-analyze_ts),]
    
    ## save the population extrema locally
    Bi <- minmax(out[,2])
    Ii <- minmax(out[,3])
    Ti <- minmax(out[,4])
    
    ## add the results to the overall vectors
    By <- c(By, Bi)
    Iy <- c(Iy, Ii)
    Ty <- c(Ty, Ti)
    
    ## add the q-values to the overall vectors
    Bx <- c(Bx, rep(qrange[i], length(Bi)))
    Ix <- c(Ix, rep(qrange[i], length(Ii)))
    Tx <- c(Tx, rep(qrange[i], length(Ti)))
    
    # print(i)
  }
  
  ## save results to harddrive
  write.csv(
    list(Bx = Bx,
         By = By
    ),
    paste(output_path, "bifout_basal.csv", sep =""),
    row.names = F
  )
  
  write.csv(
    list(Ix = Ix,
         Iy = Iy
    ),
    paste(output_path, "bifout_intermediate.csv", sep =""),
    row.names = F
  )
  
  write.csv(
    list(Tx = Tx,
         Ty = Ty
    ),
    paste(output_path, "bifout_top.csv", sep =""),
    row.names = F
  )
  
}



