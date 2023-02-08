################################################################################
#    Function "run_foodweb_sim_mt" to run the 10-species food web across       #
#    a gradient of q values in parallel                                        #
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
# Martinez (2004) with minor changes. See also Kalinkat et al. (2023) and Rall
# et al. (2023) for details. The package odeintr (Keitt 2017) is required to run
# the function.
#
# Arguments:
# qrange:          A vector with all q-values to be simulated (default = seq(0, 4, length = 201)
# a:               The allometric constant (default = 0.2227; see Otto et al. 2007)
# b:               The allometric exponent (default = -0.25; see Otto et al. 2007)
# e:               The assimilation efficiency (default = 0.85; see Otto et al. 2007)
# y:               The relative maximum feeding rate (default = 8; see Otto et al. 2007)
# N0:              The half saturation rate (default = 0.5; see Otto et al. 2007)
# R:               The body mass ratio range a species can have (default = c(10, 100)). Must be a vector of 2 numbers!
# ts_runs:         The number of sequential simulation runs adding up to on time series (default = 10)
# ts_run_length:   The length of one separated simulation from all runs adding up to one time series (default = 500)
# steplength:      The explicit times steps that will be simulated (default = 1)
# output_path:     The (sub-)path were the output files should be saved to (default = "SIM_OUT/")
# output_filename: The name of the output file (default = "strong_interactions.csv")
# noC:             The number of threads that should be used (default to 3/4 of all possible threads)
#                  NOTE: if you want to run the code on a single core use the function run_foodweb_sim()
# 
# Requirements:
# Compilation of the foodweb model (run compile_foodweb()) and the function
# "run_foodchain_sim()" that is called here.
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr, Keitt 2017)
# Needs the package parallel
# Needs the package foreach (https://CRAN.R-project.org/package=foreach)
# Needs the package iterators (https://CRAN.R-project.org/package=iterators)
# Needs the package doParallel (https://CRAN.R-project.org/package=doParallel)
#
# References
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11: 1033818. https://doi.org/10.3389/fevo.2023.1033818
# Keitt (2017): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71; https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359
# Rall et al. (2023): Rare type III responses: Code & modelling methods (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1
#
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

  ## check if the output folder exists, if not create it:
  if(!dir.exists(output_path)){
    dir.create(output_path)
    warning("The output path does not exist - I'll create it for you.")
  }
  
  ## split the qrange in approximately equally-sized sub-vectors for parallelization:
  qrange <- sample(qrange)                                                      # randomize qrange to randomly assign q-values to threads
  splits_q <- rep(floor(length(qrange)/noC), noC-1)                             # create noC-1 equally spaced ranges
  splits_q <- cumsum(splits_q)                                                  # cumulative sum of the equally spaced ranges (e.g. c(3,3,3) becomes c(3,6,9))
  splits_q <- c(0, splits_q, (length(qrange)-max(splits_q))+max(splits_q))      # add position 0 and something that works....
  
  qranges <- foreach::foreach(i = 1:noC) %do% {                                 # separate the qrange in several qranges for parallel computing
    qrange[(splits_q[i]+1):splits_q[i+1]]
  }
  
  cl <- parallel::makeCluster(noC)                                              # setup the cluster on your computer (allows to use more than one core; see e.g. https://privefl.github.io/blog/a-guide-to-parallelism-in-r/)
  doParallel::registerDoParallel(cl)                                            # register the cluster (see e.g. https://privefl.github.io/blog/a-guide-to-parallelism-in-r/)
  out <- foreach::foreach(i = 1:noC,                                            # run the foreach loop across n cores (noC)
                   .combine = "rbind",                                          # data will be collected as new rows in a dataset/list/matrix type
                   .packages = "odeintr",                                       # export the odintr package to all cores (just for safety)
                   .export = c("compile_foodweb",                               # export required self-written functions to all cores (just for safety)
                               "run_foodweb_sim",
                               "set_foodweb_parms")) %dopar% {
    compile_foodweb()                                                           # compile the foodweb on each core
                     
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
                    create_csv = F,                                             # no export of the csv for each core!
                    create.folder = F)                                          # no creation of a new folder needed
  }
  parallel::stopCluster(cl)                                                     # stop the cluster (from here only one core is used)
  
  ## save results to hard-drive
  write.csv(out,
            paste(output_path, output_filename, sep =""),
            row.names = F)                                                      # save results on hard drive
}



