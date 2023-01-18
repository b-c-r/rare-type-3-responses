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

## setup
rm(list = ls())                                                                 # clean your workspace
library("odeintr")                                                              # load library odeintr (Keitt 2017)

## load functions
path <- "FUNCTIONS/"                                                            # path in which to find the functions
fnames <- list.files(path = path, pattern = ".R")                               # saves all function names
for(i in 1:length(fnames)){
  source(paste(path, fnames[i], sep = ""))                                      # reads all source files
}

## compile the food-chain model
compile_foodchain()

## run the simulations for the bifurcation output
run_foodchain_sim()

