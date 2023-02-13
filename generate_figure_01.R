################################################################################
#    Script for creating figure 1 for "Empirical evidence of type III          #
#    functional responses and why it remains rare" by Kalinkat et al. (2023)   #
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

################################################################################
## Overview                                                                   ##
################################################################################

# With this script you can re-create figure 1 from Kalinkat et al. (2023). For
# more details on the methodology see Rall et al. (2023).
#
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11: 1033818. https://doi.org/10.3389/fevo.2023.1033818
# Rall et al. (2023): Rare type III responses: methods for code and simulation models (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
#

################################################################################
## setup                                                                      ##
################################################################################

## housekeeping
rm(list = ls())                                                                 # clean your workspace

## load required packages
library("odeintr")                                                              # load library odeintr (Keitt 2017) for compiling the model
library("parallel")                                                             # for parallel computing
library("foreach")                                                              # for parallel computing
library("iterators")                                                            # for parallel computing
library("doParallel")                                                           # for parallel computing
library("mgcv")                                                                 # for biodiversity analyses

## load functions
path <- "FUNCTIONS/"                                                            # path in which to find the functions
fnames <- list.files(path = path, pattern = ".R")                               # saves all function names
for(i in 1:length(fnames)){
  source(paste(path, fnames[i], sep = ""))                                      # reads all source files
}

## set a seed for reproducibility
set.seed(111)

################################################################################
## simulations for Figure 1E/F from Kalinkat et al. (2023)                    ##
################################################################################

## run the food chain simulations for the bifurcation diagram
## (see https://doi.org/10.5281/zenodo.7619822 for details on settings)
system.time({
  run_foodchain_sim_mt(qrange = seq(0, .2, length = 1800),                      # range of q-value
                       ts_length = 150000,                                      # total length of each timeseries
                       steplength = .5,                                         # initial step length
                       analyze_ts = 0.05,                                       # the range of the time series that should be analyzed (here the last 5%)
                       unique_out = T,                                          # only unique extrema should be saved
                       max_out = 0,                                             # saves all extrema, leads to large out files (each >6MB)
                       output_path = "SIM_OUT/",                                # saves the output files in "SIM_OUt/" - will be generated if not existing
                       noC = 12)                                                # Number of cores that should be used. Use parallel::detectCores() to see how many threads you have. Consider to not use all if you need your computer in the meanwhile.
})

## run the food web simulations for the strong interactions biodiversity diagram
## (see https://doi.org/10.5281/zenodo.7619822 for details on settings)
system.time({
  run_foodweb_sim_mt(qrange = seq(0, 4, length = 600),                          # range of q-value
                     a = 0.2227,                                                # the allometric constant
                     b = -0.25,                                                 # the allometric exponent
                     e = 0.85,                                                  # the assimilation efficiency
                     y = 8,                                                     # the relative maximum feeding rate
                     N0 = .5,                                                   # the half saturation density
                     Rrange = c(10,100),                                        # the range of (average) body mass ratios a consumer species in the food web can have
                     ts_runs = 20,                                              # number of sub-timeseries that should be analyzed. 
                     ts_run_length = 1000,                                      # total length of each sub-timeseries. e.g. 20 sub-timeseries with each 1000 time-steps sum up to a 20,000 timesteps timeseries.
                     steplength = .5,                                           # the steplength-length. Here 2000 steps of length of 0.5 create 1000 time steps.
                     output_path = "SIM_OUT/",                                  # saves the output files in "SIM_OUt/" - will be generated if not existing
                     output_filename = "strong_interactions.csv",               # output file name
                     noC = 12)                                                  # Number of cores that should be used. Use parallel::detectCores() to see how many threads you have. Consider to not use all if you need your computer in the meanwhile.
})

## run the food web simulations for the weak interactions biodiversity diagram
## (see https://doi.org/10.5281/zenodo.7619822 for details on settings)
system.time({
  run_foodweb_sim_mt(qrange = seq(0, 4, length = 600),                          # range of q-value
                     a = 0.2227,                                                # the allometric constant
                     b = -0.25,                                                 # the allometric exponent
                     e = 0.85,                                                  # the assimilation efficiency
                     y = 4,                                                     # the relative maximum feeding rate
                     N0 = 1,                                                    # the half saturation density
                     Rrange = c(10,100),                                        # the range of (average) body mass ratios a consumer species in the food web can have
                     ts_runs = 20,                                              # number of sub-timeseries that should be analyzed. 
                     ts_run_length = 1000,                                      # saves the output files in "SIM_OUt/" - will be generated if not existing
                     steplength = .5,                                           # the steplength-length. Here 2000 steps of length of 0.5 create 1000 time steps.
                     output_path = "SIM_OUT/",                                  # saves the output files in "SIM_OUt/" - will be generated if not existing
                     output_filename = "weak_interactions.csv",                 # output file name
                     noC = 12)                                                  # Number of cores that should be used. Use parallel::detectCores() to see how many threads you have. Consider to not use all if you need your computer in the meanwhile.
})


################################################################################
## generate figure 01 with all 6 subplots from Kalinkat et al. (2023        ) ##
################################################################################

## pdf setup
dir.create("FIG_OUT/")                                                          # creates an empty directory for the output pdf
pdf(file = "FIG_OUT/Figure01.pdf",
    width = 6,
    height = 7.5,
    title = "Figure 1: Rare Type III Responses")                                # pdf settings, see ?pdf for details

## Graphical settings
par(mfrow = c(3,2),
    oma = c(.1,.1,.1,.1),
    mar = c(4,4,.5,.5),
    las = 1)                                                                    # graphical settings to create a nice 2*3 ploting area

## create the figure
# calls functions to create respective sub-plots
create_fig01A(save_output = F)
create_fig01B(save_output = F)
create_fig01C(save_output = F)
create_fig01D(save_output = F)
create_fig01E(save_output = F)
create_fig01F(save_output = F)
dev.off()                                                                       # closes pdf

writeLines(capture.output(sessionInfo()), "sessionInfo.txt")                    # saves information on your R installation for reproducibility
