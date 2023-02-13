################################################################################
#    Function "run_foodchain_sim" to run the 3-species food chain across       #
#    a gradient of q values                                                    #
#                                                                              #
#    Copyright (C) 2023 Björn C. Rall (b-c-r@mailbox.org)                      #
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
# The function simulates the 3-species food chain across a gradient of q-values
# and saves the output as *.csv files for a bifurcation diagram. The food-chain
# model is based on Otto et al. (2007) combined with ideas from Williams and
# Martinez (2004). See also Kalinkat et al. (2023) and Rall et al. (2023) for
# details. The package odeintr (Keitt 2017) is required to run the function.
#
# Arguments:
# qrange:        A vector with all q-values to be simulated (default = seq(0, .2, length = 501)
# ts_length:     The length of each time series to be simulated (default = 100000)
# steplength:    The explicit times steps that will be simulated (default = .5)
# analyze_ts:    The percentage of the time series that should be analyzed (e.g.
#                    the last 5 % [0.05]) (default = 0.05; between 0 and 1)
# unique_out:    Should double entries within one q value be deleted? (default = T),
# max_out:       Number of different extrema to be saved (default = 20; 0 == saves all),
# output_path:   The (sub-)path were the output files should be saved to (default = "SIM_OUT/")
# create.folder: Should the function create an output folder? Default = T.
# 
# Requirements:
# Compilation of the food chain model (run compile_foodchain())
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr, Keitt 2017)
# 
# References
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11: 1033818. https://doi.org/10.3389/fevo.2023.1033818
# Keitt (2017): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71; https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359.
# Rall et al. (2023): Rare type III responses: methods for code and simulation models (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1.#

run_foodchain_sim <- function(qrange = seq(0, .2, length = 501),
                              ts_length = 100000,
                              steplength = .5,
                              analyze_ts = 0.05,
                              unique_out = T,
                              max_out = 20,
                              output_path = "SIM_OUT/",
                              create.folder = T){

  ## check if the output folder exists, if not create it:
  if(create.folder){
    if(!dir.exists(output_path)){
      dir.create(output_path)
      warning("The output path does not exist - I'll create it for you.")
    }
  }

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
    out <- chainC_adap(c(runif(1, .1, 1),                                       # random initial biomass densities (basal)
                         runif(1, .1, 1),                                       # random initial biomass densities (intermediate)
                         runif(1, .1, 1)),                                      # random initial biomass densities (top)
                       ts_length,
                       steplength)
    
    ## cut the time series for further analyses and save only the last x pecent:
    out <- out[out$Time > max(out$Time)*(1-analyze_ts),]
    
    ## save the population extrema locally
    Bi <- minmax(out[,2])
    Ii <- minmax(out[,3])
    Ti <- minmax(out[,4])
    
    ## reduce to unique extrema
    if(unique_out == T){
      Bi <- unique(Bi)
      Ii <- unique(Ii)
      Ti <- unique(Ti)      
    }
    
    ## reduce output data
    if(max_out > 0){
      if(length(Bi) > max_out){
        Bi <- sample(Bi, max_out)
      }
      if(length(Ii) > max_out){
        Ii <- sample(Ii, max_out)
      }
      if(length(Ti) > max_out){
        Ti <- sample(Ti, max_out)
      }
    }
    
    
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
  
  ## save results to hard-drive
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



