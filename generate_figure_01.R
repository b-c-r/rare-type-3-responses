################################################################################
#    Script to simulate the data for a bifurcation diagram of a                #
#    3-species food chain across a gradient of q-values                        #
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

## load functions
path <- "FUNCTIONS/"                                                            # path in which to find the functions
fnames <- list.files(path = path, pattern = ".R")                               # saves all function names
for(i in 1:length(fnames)){
  source(paste(path, fnames[i], sep = ""))                                      # reads all source files
}

################################################################################
## simulations for Figure 1E/F from Kalinkat et al. (under review)            ##
################################################################################

## run the food chain simulations for the bifurcation diagram
system.time({
  run_foodchain_sim_mt(qrange = seq(0, .2, length = 501),                       # default: length = 2001
                                               ts_length = 50000,                                       # default: 200000
                                               steplength = 1,                                          # default: .5 
                                               analyze_ts = 0.2)                                                        
})
# Note that executing this function is time expensive
# I ran ~ 475sec (~8min) on a AMD 6900HS Creators Edition using 12/16 threads
# You may consider to run shorter time series and a coarser resolution:
# system.time({
#   run_foodchain_sim_mt(qrange = seq(0, .2, length = 501),                       # default: length = 2001
#                        ts_length = 50000,                                       # default: 200000
#                        steplength = 1,                                          # default: .5 
#                        analyze_ts = 0.2)                                        # default: .05
#})


################################################################################
## generate figure 01 with all 6 subplots from Kalinkat et al. (under review) ##
################################################################################

## pdf setup
dir.create("FIG_OUT/")
pdf(file = "FIG_OUT/Figure01.pdf",
    width = 7,
    height = 4.8,
    title = "Figure 1: Rare Type III Responses")

## Graphical settings
par(mfrow = c(2,3),
    oma = c(.1,.1,.1,.1),
    mar = c(4,4,.5,.5),
    las = 1)

## create the figure
create_fig01A(save_output = F)
create_fig01B(save_output = F)
create_fig01C(save_output = F)
create_fig01D(save_output = F)
create_fig01E(save_output = F)

dev.off()


