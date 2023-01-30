################################################################################
#    Script for creating figure 1 for "Empirical evidence of type III          #
#    functional responses and why it remains rare"                             #
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
library("mgcv")                                                                 # for biodiversity analyses

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
  run_foodchain_sim_mt(qrange = seq(0, .2, length = 1800),
                       ts_length = 150000,
                       steplength = .5,
                       analyze_ts = 0.05,
                       unique_out = T,
                       max_out = 0,                                             # saves all extrema, leads to large out files (each >6MB)
                       output_path = "SIM_OUT/",
                       noC = 12)
}) # ~ 4-7min with AMD Ryzen 6000 series laptop

## run the food web simulations for the strong interactions biodiversity diagram
system.time({
  run_foodweb_sim_mt(qrange = seq(0, 4, length = 600),
                     a = 0.2227,
                     b = -0.25,
                     e = 0.85,
                     y = 8,
                     N0 = .5,
                     Rrange = c(10,100),
                     ts_runs = 20,
                     ts_run_length = 1000,
                     steplength = .5,
                     output_path = "SIM_OUT/",
                     output_filename = "strong_interactions.csv",
                     noC = 12)
}) # ~ 9-11 min with AMD Ryzen 6000 series laptop

## run the food web simulations for the weak interactions biodiversity diagram
system.time({
  run_foodweb_sim_mt(qrange = seq(0, 4, length = 600),
                     a = 0.2227,
                     b = -0.25,
                     e = 0.85,
                     y = 4,
                     N0 = 1,
                     Rrange = c(10,100),
                     ts_runs = 20,
                     ts_run_length = 1000,
                     steplength = .5,
                     output_path = "SIM_OUT/",
                     output_filename = "weak_interactions.csv",
                     noC = 12)
}) # ~ 2-3 min with AMD Ryzen 6000 series laptop


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
create_fig01C_alt(save_output = F)
create_fig01D(save_output = F)
create_fig01E(save_output = F)
create_fig01F(save_output = F)
dev.off()

writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
