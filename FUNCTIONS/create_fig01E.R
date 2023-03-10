################################################################################
#    Function "create_fig01E" to create Figure 01E for Kalinkat et al. (2023)  #
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
#
# Description:
# The function creates Figure 1(E) from Kalinkat et al. (2023), see Rall et al.
# (2023) for details.
#
# Arguments:
# B_in:        Bifurcation data for the basal species. The *.CSV name (default = "bifout_basal.csv")
# I_in:        Bifurcation data for the intermediate species. The *.CSV name (default = "bifout_intermediate.csv")
# T_in:        Bifurcation data for the top species. The *.CSV name (default = "bifout_top.csv")
# input_path:  the (sub-)path were the output files should be saved to (default = "SIM_OUT/")
# output_path: the (sub-)path were the output files should be saved to (default = "FIG_OUT/")
# save_output: Logical (T/F). Should the output be saved as pdf?
# 
# Requirements:
# Needs bifurcation data from run_foodchain_sim_mt().
# 
# References
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11: 1033818. https://doi.org/10.3389/fevo.2023.1033818
# Rall et al. (2023): Rare type III responses: methods for code and simulation models (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# 

create_fig01E <- function(B_in = "bifout_basal.csv",
                          I_in = "bifout_intermediate.csv",
                          T_in = "bifout_top.csv",
                          input_path = "SIM_OUT/",
                          output_path = "FIG_OUT/",
                          save_output = T){
  
  ## check if the output folder exists, if not create it:
  if(!dir.exists(output_path)){
    dir.create(output_path)
    warning("The output path does not exist - I'll create it for you.")
  }
  
  ## import bifurcation data
  Basal <- read.csv(paste(input_path, B_in, sep = ""))
  Inter <- read.csv(paste(input_path, I_in, sep = ""))
  Top <- read.csv(paste(input_path, T_in, sep = ""))
  
  if(save_output) {pdf(file = paste(output_path, "Figure01E.pdf", sep =""))}
  
  par(las=1)
  plot(Top$Tx+1, Top$Ty,
       pch = 16, cex = .2, 
       ylim = c(0,5), col = "red",
       xlab = expression(Theta),
       ylab = "Population Density Extrema")
  points(Inter$Ix+1, Inter$Iy, pch = 16, cex = .2, col = "orange")
  points(Basal$Bx+1, Basal$By, pch = 16, cex = .2, col = "blue")
  mtext("(E)",
        line = -1.5,
        adj = 0.05)
  legend("topright",
         legend = c("Top Consumer",
                    "Intermediate Consumer",
                    "Basal Resource"),
         col = c("red",
                      "orange",
                      "blue"),
                      pch = 16,
         cex = 0.75)
  
  if(save_output) {dev.off()}
  
}



