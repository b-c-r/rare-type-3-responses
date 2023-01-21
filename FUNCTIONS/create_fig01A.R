################################################################################
#    Function "create_fig01D" to create Figure 01 (A) from                     #
#        Kalinkat et al. (under review)                                        #
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
# The function creates Figure 1(A) from Kalinkat et al. (under review)
#
# Arguments:
# N:     The resource densities (positive vector)
# Fmax:  The maximum feeding rate (the curves' asymptote, single value)
# a:     The attack rate (single value)
# ylims: the limits of the y-axis
# output_path: the (sub-)path were the output files should be saved to (default = "FIG_OUT/")
# save_output: Logical (T/F). Should the output be saved as pdf?
# 
# The default settings are as used in Kalinkat et al. (under review).
# 
# Requirements:
# None
# 
# References
# Kalinkat et al. (under review): Empirical evidence of type III functional responses and why it remains rare.
# 

create_fig01A <- function(N = seq(0, 20, length=1000),
                          Fmax = 10,
                          a = 3,
                          ylims = c(0,15),
                          output_path = "FIG_OUT/",
                          save_output = T){
  
  if(!dir.exists(output_path)){
    dir.create(output_path)
    warning("The output path does not exist - I'll create it for you.")
  }
  
  type00 <- a*N
  type01 <- ifelse(type00 > Fmax, Fmax, type00)
  
  if(save_output) {pdf(file = paste(output_path, "Figure01A.pdf", sep =""))}
  
  par(las=1)
  plot(N,
       type01,
       type = "l",
       ylim = ylims,
       xlab = "resource density",
       ylab = "feeding rate / predation risk"
  )
  lines(N, type00, lty = 2)
  lines(N, type01/N, lty = 1, col = "darkgrey")
  lines(N, type00/N, lty = 2, col = "darkgrey")
  mtext("(A)",
        line = -1.5,
        adj = 0.05)
  legend("topright",
         legend = c("Type I feeding rate",
                    "Type I pred. risk",
                    "Type 0 feeding rate",
                    "Type 0 pred. risk"),
         col = c("black",
                        "darkgrey",
                        "black",
                        "darkgrey"),
                        lty = c(1,1,2,2),
         cex = 0.75)
  
  if(save_output) {dev.off()}
  
}
