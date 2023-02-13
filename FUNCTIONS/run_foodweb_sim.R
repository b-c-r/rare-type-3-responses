################################################################################
#    Function "run_foodchain_sim" to run the 10-species food web across        #
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
# The function simulates the 10-species food web across a gradient of q-values
# and saves the output as *.csv files. The foodweb model is based on Williams and
# Martinez (2004) paper with minor changes. See also Kalinkat et al. (2023) and 
# Rall et al. (2023) for details. The package odeintr (Keitt 2017) is required
# to run the function.
#
# Arguments:
# qrange:          a vector with all q-values to be simulated (default = seq(0, 4, length = 201)
# a:               the allometric constant (default = 0.2227; see Otto et al. 2007)
# b:               the allometric exponent (default = -0.25; see Otto et al. 2007)
# e:               the assimilation efficiency (default = 0.85; see Otto et al. 2007)
# y:               the relative maximum feeding rate (default = 8; see Otto et al. 2007)
# N0:              the half saturation rate (default = 0.5; see Otto et al. 2007)
# R:               the body mass ratio range a species can have (default = c(10, 100)). Must be a vector of 2 numbers!
# ts_runs:         the number of sequential simulation runs adding up to on time series (default = 10)
# ts_run_length:   the length of one separated simulation from all runs adding up to one time series (default = 500)
# steplength:      the explicit times steps that will be simulated (default = 1)
# output_path:     the (sub-)path were the output files should be saved to (default = "SIM_OUT/")
# output_filename: the name of the output file (default = "strong_interactions.csv")
# create_csv:      logical: should a csv be written to the hard-drive? (default = T)
# create_folder:   logical: should a new path be generated (default = T)
# 
# Requirements:
# Compilation of the foodweb model (run compile_foodweb())
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr; Keitt 2017)
# 
# References
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11: 1033818. https://doi.org/10.3389/fevo.2023.1033818
# Keitt (2017): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71; https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359.
# Rall et al. (2023): Rare type III responses: methods for code and simulation models (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1.
#

run_foodweb_sim <- function(qrange = seq(0, 4, length = 201),
                            a = 0.2227,
                            b = -0.25,
                            e = 0.85,
                            y = 8,
                            N0 = .5,
                            Rrange = c(10,100),
                            ts_runs = 10,
                            ts_run_length = 500,
                            steplength = 1,
                            output_path = "SIM_OUT/",
                            output_filename = "strong_interactions.csv",
                            create_csv = T,
                            create.folder = T){
  
  ## check if the output folder exists, if not create it:
  if(create.folder){
    if(!dir.exists(output_path)){
      dir.create(output_path)
      warning("The output path does not exist - I'll create it for you.")
    }
  }
  
  divs <- c()                                                                   ## object to store the current biodiversity levels
  
  ## run the simulations across the qrange vector:
  for(i in 1:length(qrange)){
    
    ## setup the model parameter values
    set_foodweb_parms(a = a,
                      b = b,
                      e = e,
                      y = y,
                      N0 = N0,
                      q = qrange[i],
                      Rrange = Rrange)
    
    ## run the simulation
    for(j in 1:ts_runs){
      out <- NULL                                                               ## a clean object to store the simulation output
      if(j == 1){                                                               ## is this the initial simulation run?
        init <- runif(10, .1, 1)                                                ## ten initial biomass densities (only for the first run of the time series)
      }
      out <- foodweb(init, ts_run_length, steplength)                           ## simulate the partial time series
      for(k in 1:10){                                                           ## check if the any species is extinct (extinction if biomass density is smaller than 1e-10)
        if(any(out[,(k+1)] < 1e-10)){
          init[k] <- 0                                                          ## if a species is extinct, set the initial biomass density to "0"
        } else{
          init[k] <- out[nrow(out),(k+1)]                                       ## if a species is alive, set the initial biomass density to the last value of the current time series
        }
      }
    }
    
    ## count the biodiversity (at the end of the simulation)
    cumdiv <- 0                                                                 ## the cumulative species richness (starting at 0)
    for(j in 1:10){                                                             ## loop over all 10 species
      if(any(out[,(j+1)] > 1e-10)){                                             ## check if the species is alive (> 1e-10)
        cumdiv <- cumdiv + 1                                                    ## add up if the species is alive
      }
    }
    divs[i] <- cumdiv                                                           ## transfer the diversity of the current simulation (cumdiv) to the overall diverstiy vector
    
  }
  
  ## output
  if(create_csv){
    write.csv(
      data.frame(q = qrange,
                 div = divs
      ),
      paste(output_path, output_filename, sep =""),
      row.names = F
    )                                                                           ## save the biodiversity results to a file (on the hard-drive)
  } else{
    return(data.frame(q = qrange,
                      div = divs
    ))                                                                          ## returns the biodiversity results to the R environment
  }
}



