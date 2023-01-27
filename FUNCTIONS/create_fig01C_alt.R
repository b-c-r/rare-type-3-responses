################################################################################
#    Function "create_fig01C_alt" to create alternative Figure 01 (C) from     #
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
# The function creates Figure 1(C alternative) from Kalinkat et al. (under review)
#
# Arguments:
# N:           The resource densities (positive vector)
# Fmax:        The maximum feeding rate (the curves' asymptote, single value)
# N0:          The half saturation rate (density at which Fmax is reached, single value)
# ylims:       The limits of the y-axis
# Nlow:        Lower cutoff as often seen in lab experiments (Sarnelle and Wilson 2008)
# Nhigh:       Upper cuttof as often seen in field observations (Coblentz et al. 2022)
# output_path: The (sub-)path were the output files should be saved to (default = "FIG_OUT/")
# save_output: Logical (T/F). Should the output be saved as pdf?
# 
# The default settings are as used in Kalinkat et al. (under review).
# 
# Requirements:
# Calls the function calc_gen_fr()
# 
# References
# Sarnelle and Wilson (2008): Type III functional response in Daphnia. Ecology 89: 1723-1732; https://doi.org/10.1890/07-0935.1
# Kalinkat et al. (under review): Empirical evidence of type III functional responses and why it remains rare.
# Coblentz et al. (2022): Predator feeding rates may often be unsaturated under typical prey densities. Ecol Lett 00: 1-11; https://doi.org/10.1111/ele.14151
# 

create_fig01C_alt <- function(N = seq(0, 20, length=1000),
                              Fmax = 10,
                              N0 = 10/3,
                              ylims = c(0,15),
                              Nlow = 2,
                              Nhigh = 5,
                              output_path = "FIG_OUT/",
                              save_output = T){
  
  if(!dir.exists(output_path)){
    dir.create(output_path)
    warning("The output path does not exist - I'll create it for you.")
  }
  
  out <- calc_gen_fr(N = N,
                     Fmax = Fmax,
                     N0 = N0,
                     Theta = 2)
  
  if(save_output) {pdf(file = paste(output_path, "Figure01C.pdf", sep =""))}
  
  par(las=1)
  plot(N,
       out[,2],
       type = "l",
       ylim = ylims,
       xlab = "resource density",
       ylab = "feeding rate / predation risk",
       lwd = 3
  )
  lines(N[N<Nlow], out[N<Nlow,2], col = "orange", lty = 1, lwd = 1)
  lines(N[N>Nhigh], out[N>Nhigh,2], col = "red", lty = 1, lwd = 1)
  
  lines(N, out[,2]/N, col = "darkgrey")
  
  mtext("(C)",
        line = -1.5,
        adj = 0.05)
  legend("topright",
         legend = c("Type III feeding rate",
                    "Type III pred. risk",
                    "often absent in laboratory",
                    "often absent in field",
                    "resulting Type I with missing data",
                    "resulting Type 0 with missing data"),
         col = c("black",
                 "darkgrey",
                 "orange",
                 "red",
                 "blue",
                 "yellow"),
         lty = c(1,1,1,1,6,2),
         lwd = c(1,1,1,1,1,1),
         cex = 0.6)
  
  ##############################################################################
  # THIS IS NOT A REAL FIT - ONLY TO SHOW THE PATTERN!!!!
  # DO NOT APPLY THIS METHOD TO REAL DATA!!!!
  
  # fitting H0 to reduced data
  N2 <- N[N>Nlow & N<Nhigh]
  y2 <- out[N>Nlow & N<Nhigh,2]
  m2 <- lm(y2~N2-1)
  # fitting saturation to upper end data
  N3 <- N[N>Nhigh]
  y3 <- out[N>Nhigh,2]
  m3 <- lm(y3~1)  
  
  # THIS IS NOT A REAL FIT - ONLY TO SHOW THE PATTERN!!!!
  # DO NOT APPLY THIS METHOD TO REAL DATA!!!!
  ##############################################################################
  
  # constructing the H1.
  N4 <- unique(sort(c(N2, N3[N3<coef(m3)[]/coef(m2)[]])))                       ## increasing part of H1, above H0 with field ranges
  N5 <- N3[N3>coef(m3)[]/coef(m2)[]]                                            ## saturating part of H1
  
  ## draw the lines
  lines(N4, predict(m2, newdata = list(N2 = N4)), col="blue",
          lwd = 1, lty = 6)                                                     # for plotting H1, increasing part
  lines(N5, rep(coef(m3)[], length(N5)), col="blue",
          lwd = 1, lty = 6)                                                     # for plotting H1, saturating part
  lines(N2, predict(m2), col="yellow", lwd = 1, lty = 2)                        # for plotting H0, the field situation
  
  # re-draw the actual H3 for better contrast
  #lines(N, out[,2])
  
  if(save_output) {dev.off()}
  
}
