################################################################################
#    Function "run_foodchain_sim" to run the 10-species food web across        #
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
# The function simulates the 10-species food web across a gradient of q values
# and saves the output as *.csv files. The foodweb model is based on Williams &
# Martinez (2004) paper with minor changes. See also the supplement information
# of Kalinkat et al. (under review) for details. The package odeintr
# (Keitt 2007) is required to run the function.
#
# Arguments:
# qrange:          a vector with all q-values to be simulated (default = seq(0, 4, length = 201)
# a:               the allometric constant (default = 0.2227; see Otto et al. 2008)
# b:               the allometric exponent (default = -0.25; see Otto et al. 2008)
# e:               the assimilation efficiency (default = 0.85; see Otto et al. 2008)
# y:               the relative maximum feeding rate (default = 8; see Otto et al. 2008)
# N0:              the half saturation rate (default = 0.5; see Otto et al. 2008)
# R:               the body mass ratio (default = c(10, 100))
# ts_runs:         the number of sequential simulation runs adding up to on time series (default = 10)
# ts_run_length:   the length of one separated simulation from all runs adding up to one time series (default = 500)
# steplength:      the explicit times steps that will be simulated (default = 1)
# output_path:     the (sub-)path were the output files should be saved to (default = "SIM_OUT/")
# output_filename: the name of the output file (default = "strong_interactions.csv")
# noC:             the number of threads that should be used (default to 3/4 of all possible)
# 
# The default settings are as used in Kalinkat et al. (under review).
# 
# Requirements:
# Compilation of the foodwe model (run compile_foodweb())
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr)
# 
# References
# Kalinkat et al. (under review): Empirical evidence of type III functional responses and why it remains rare.
# Keitt (2007): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71, https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359.
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1.
#


run_foodweb_sim_mt <- function(qrange = seq(0, 4, length = 201),
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
  out <- foreach::foreach(i = 1:noC,
                   .combine = "rbind",
                   .packages = "odeintr",
                   .export = c("compile_foodweb",
                               "run_foodweb_sim",
                               "set_foodweb_parms")) %dopar% {
    compile_foodweb()
                     
    ## run the simulation
    run_foodweb_sim(qrange = qranges[[i]],
                    a = a,
                    b = b,
                    e = e,
                    y = y,
                    N0 = N0,
                    Rrange = Rrange,
                    ts_runs = ts_runs,
                    ts_run_length = ts_run_length,
                    steplength = steplength,
                    output_path = output_path,
                    output_filename = output_filename,
                    create_csv = F,
                    create.folder = F)
  }
  parallel::stopCluster(cl)
  
  ## save results to hard-drive
  write.csv(out,
            paste(output_path, output_filename, sep =""),
            row.names = F)
}



