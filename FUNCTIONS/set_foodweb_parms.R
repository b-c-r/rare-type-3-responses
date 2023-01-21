################################################################################
#    Function "set_foodweb_parms" to set values for a 10-species food web      #
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
# The function sets the parameter values for the compiled foodweb. 
# You need to execute compile_foodweb() first! See the description of
# compile_foodweb() for the model. See also the supplement of Kalinkat
# et al. (under review) for further methodology description.
#
# Arguments:
# a:  the allometric constant (default = 0.2227; see Otto et al. 2008)
# b:  the allometric exponent (default = -0.25; see Otto et al. 2008)
# e:  the assimilation efficiency (default = 0.85; see Otto et al. 2008)
# y:  the relative maximum feeding rate (default = 8; see Otto et al. 2008)
# N0: the half saturation rate (default = 0.5; see Otto et al. 2008)
# q:  the shaping exponent (default = 0; see Williams and Martinez 2004)
# R:  the range of body mass ratios allowed for each species (default = c(10, 100))
# 
# Requirements:
# Compilation of the foodweb model (run compile_foodweb())
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr)
# 
# References
# Kalinkat et al. (under review): Empirical evidence of type III functional responses and why it remains rare.
# Keitt (2007): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71, https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359.
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1.
#

set_foodweb_parms <- function(a = 0.2227,
                              b = -0.25,
                              e = 0.85,
                              y = 8,
                              N0 = .5,
                              q = 0,
                              Rrange = c(10,100)){
  
  ## Calculate the trophic levels:
  TL <- 1
  TL[2] <- 1
  TL[3] <- 0.5*TL[1] + 0.5*TL[2] + 1
  TL[4] <- 0.5*TL[1] + 0.5*TL[2] + 1
  TL[5] <- TL[2] + 1
  TL[6] <- 0.5*TL[3] + 0.5*TL[4] + 1
  TL[7] <- TL[3] + 1
  TL[8] <- 0.4*TL[4] + 0.4*TL[5] + 0.2*TL[6] + 1
  TL[9] <- TL[8] + 1
  TL[10] <- 0.8*TL[5] + 0.2*TL[6] + 1
  
  ## calculate body size ratios (relative to basal species)
  Rs <- c(1,1)
  for(i in 3:10){
    Rs[i] <- runif(1,Rrange[1],Rrange[2])^(TL[i]-1)
  }
  
  ## set parameter values
  foodweb_set_params(m2 = a * Rs[3]^b,
                     m3 = a * Rs[4]^b,
                     m4 = a * Rs[5]^b,
                     m5 = a * Rs[6]^b,
                     m6 = a * Rs[7]^b,
                     m7 = a * Rs[8]^b,
                     m8 = a * Rs[9]^b,
                     m9 = a * Rs[10]^b,
                     e = e,
                     y = y,
                     N0 = N0,
                     q = q
  )
  
}



