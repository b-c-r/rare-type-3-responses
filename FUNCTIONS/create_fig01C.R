################################################################################
#    Function "create_fig01C" to create Figure 01C for Kalinkat et al. (2023)  #
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
# The function creates Figure 1(C) from Kalinkat et al. (2023), see Rall et al.
# (2023) for details.
#
# Arguments:
# N:           The resource densities (positive vector)
# Fmax:        The maximum feeding rate (the curves' asymptotic maximum, single value)
# N0:          The half saturation rate (density at which Fmax is reached, single value)
# ylims:       The limits of the y-axis
# Nlow:        Lower cutoff as often seen in lab experiments (Sarnelle and Wilson 2008)
# Nhigh:       Upper cutoff as often seen in field observations (Coblentz et al. 2022)
# output_path: The (sub-)path were the output files should be saved to (default = "FIG_OUT/")
# save_output: Logical (T/F). Should the output be saved as pdf?
# 
# Requirements:
# Calls the function calc_gen_fr()
# 
# References
# Coblentz et al. (2022): Predator feeding rates may often be unsaturated under typical prey densities. Ecol Lett 00: 1-11; https://doi.org/10.1111/ele.14151
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11: 1033818. https://doi.org/10.3389/fevo.2023.1033818
# Rall et al. (2023): Rare type III responses: methods for code and simulation models (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# Sarnelle and Wilson (2008): Type III functional response in Daphnia. Ecology 89: 1723-1732; https://doi.org/10.1890/07-0935.1
#

create_fig01C <- function(N = seq(0, 20, length=1000),
                          Fmax = 10,
                          N0 = 10/3,
                          ylims = c(0,15),
                          Nlow = 2,
                          Nhigh = 5,
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
                     Theta = 2)
  
  if(save_output) {pdf(file = paste(output_path, "Figure01C.pdf", sep =""))}
  
  par(las=1)
  plot(N,
       out[,2],
       type = "l",
       ylim = ylims,
       xlab = "resource density",
       ylab = "feeding rate / predation risk",
       lwd = 4,
       col = "grey"
  )
  lines(N[N<Nlow], out[N<Nlow,2], col = "orange", lty = 1, lwd = 1)
  lines(N[N>Nhigh], out[N>Nhigh,2], col = "red", lty = 1, lwd = 1)
  
  lines(N, out[,2]/N, col = "black")
  
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
         col = c("grey",
                 "black",
                 "orange",
                 "red",
                 "blue",
                 "magenta1"),
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
  lines(c(0, N4)+0.15, predict(m2, newdata = list(N2 = c(0, N4))), col="blue",
          lwd = 1, lty = 6)                                                     # for plotting H1, increasing part
  lines(N5+0.15, rep(coef(m3)[], length(N5)), col="blue",
          lwd = 1, lty = 6)                                                     # for plotting H1, saturating part
  lines(c(0, N4)-0.15, predict(m2, newdata = list(N2 = c(0, N4))),
        col="magenta1", lwd = 1, lty = 2)                                         # for plotting H0, the field situation
  
  # re-draw the actual H3 for better contrast
  # lines(N, out[,2])
  
  if(save_output) {dev.off()}
  
}
