################################################################################
#    Function "create_fig01B" to create Figure 01B for Kalinkat et al. (2023)  #
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
# The function creates Figure 1(B) from Kalinkat et al. (2023), see Rall et al.
# (2023) for more information.
#
# Arguments:
# N:           The resource densities (positive vector)
# Fmax:        The maximum feeding rate (the curves' asymptote, single value)
# N0:          The half saturation rate (density at which Fmax is reached, single value)
# ylims:       The limits of the y-axis
# output_path: The (sub-)path were the output files should be saved to (default = "FIG_OUT/")
# save_output: Logical (T/F). Should the output be saved as pdf?
# 
# Requirements:
# Calls the function calc_gen_fr()
# 
# References
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11: 1033818. https://doi.org/10.3389/fevo.2023.1033818
# Rall et al. (2023): Rare type III responses: methods for code and simulation models (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# 

create_fig01B <- function(N = seq(0, 20, length=1000),
                          Fmax = 10,
                          N0 = 10/3,
                          ylims = c(0,15),
                          output_path = "FIG_OUT/",
                          save_output = T){
  
  ## check if the output folder exists, if not create it:
  if(!dir.exists(output_path)){
    dir.create(output_path)
    warning("The output path does not exist - I'll create it for you.")
  }
  
  out <- calc_gen_fr(N = N,
                     Fmax = Fmax,
                     N0 = N0,
                     Theta = 1)
  
  if(save_output) {pdf(file = paste(output_path, "Figure01B.pdf", sep =""))}
  
  par(las=1)
  plot(N,
       out[,2],
       type = "l",
       ylim = ylims,
       xlab = "resource density",
       ylab = "feeding rate / predation risk",
       col = "grey"
  )
  
  lines(N, out[,2]/N, col = "black")
  mtext("(B)",
        line = -1.5,
        adj = 0.05)
  legend("topright",
         legend = c("Type II feeding rate",
                    "Type II pred. risk"),
         col = c("grey",
                        "black"),
                        lty = c(1,1),
         cex = 0.75)
  
  if(save_output) {dev.off()}
  
}



