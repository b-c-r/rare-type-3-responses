################################################################################
#    Function "set_foodweb_parms" to set values for a 10-species food web      #
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
# The function sets the parameter values for the compiled foodweb. See the
# description of the odeintr package (Keitt 2017) for details how to apply
# such functions. You need to execute compile_foodweb() first! See also the 
# description in compile_foodweb.R for the model. See Kalinkat et al. (2023),
# Rall et al. (2023), and references below for further description.
#
# Arguments:
# a:      The allometric constant (default = 0.2227; see Otto et al. 2007)
# b:      The allometric exponent (default = -0.25; see Otto et al. 2007)
# e:      The assimilation efficiency (default = 0.85; see Otto et al. 2007)
# y:      The relative maximum feeding rate (default = 8; see Otto et al. 2007)
# N0:     The half saturation rate (default = 0.5; see Otto et al. 2007)
# q:      The shaping exponent (default = 0; see Williams and Martinez 2004)
# Rrange: The range of body mass ratios allowed for each species (default = c(10, 100))
# 
# Requirements:
# Compilation of the foodweb model (run compile_foodweb())
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr)
# 
# References
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11. https://doi.org/10.3389/fevo.2023.1033818
# Keitt (2017): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71; https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359.
# Rall et al. (2008): Food-web connectance and predator interference dampen the paradox of enrichment; Oikos 117: 202-213; https://doi.org/10.1111/j.2007.0030-1299.15491.x
# Rall et al. (2023): Rare type III responses: Code & modelling methods (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1.
#

set_foodweb_parms <- function(a = 0.2227,
                              b = -0.25,
                              e = 0.85,
                              y = 8,
                              N0 = .5,
                              q = 0,
                              Rrange = c(10,100)){
  
  ## Calculation of the trophic levels (TL). A consumer has a trophic level of
  ## the average TL of all its resources (Williams and Martinez 2004):
  TL <- 1                                                                       ## Species 1 (x[0])
  TL[2] <- 1                                                                    ## Species 2 (x[1])
  TL[3] <- 0.5*TL[1] + 0.5*TL[2] + 1                                            ## Species 3 (x[2])
  TL[4] <- 0.5*TL[1] + 0.5*TL[2] + 1                                            ## Species 4 (x[3])
  TL[5] <- TL[2] + 1                                                            ## Species 5 (x[4])
  TL[6] <- 0.5*TL[3] + 0.5*TL[4] + 1                                            ## Species 6 (x[5])
  TL[7] <- TL[3] + 1                                                            ## Species 7 (x[6])
  TL[8] <- 0.4*TL[4] + 0.4*TL[5] + 0.2*TL[6] + 1                                ## Species 8 (x[7])
  TL[9] <- TL[8] + 1                                                            ## Species 9 (x[8])
  TL[10] <- 0.8*TL[5] + 0.2*TL[6] + 1                                           ## Species 10 (x[10])
  
  ## calculate body size ratios (relative to basal species)
  Rs <- c(1,1)                                                                  ## Species 1+2 (x[0] + x[1]) have a size ratio of 1 
  for(i in 3:10){
    Rs[i] <- runif(1,Rrange[1],Rrange[2])^(TL[i]-1)                             ## Calculation of the body size ratio of consumer species using uniform random variance (Williams and Martinez 2004; Rall et al. 2008)
  }
  
  ## set parameter values:
  foodweb_set_params(m2 = a * Rs[3]^b,                                          ## calculation of the metabolic rates (see Rall et al. 2023)
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



