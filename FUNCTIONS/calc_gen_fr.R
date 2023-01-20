################################################################################
#    Function "calc_gen_fr" to calculate FR curves                             #
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
# The function calculates the shapes of the generalized or Theta-sigmoid
# functional response (e.g., Real 1977; Okuyama & Ruyle 2011; Vucic-Pestic et
# al. 2010). See Kalinkat et al. (under review) for details.
#
# Arguments:
# N:     The resource densities (positive vector)
# Fmax:  The maximum feeding rate (the curves' asymptote, single value)
# N0:    The half saturation rate (density at which Fmax is reached, single value)
# theta: The shaping exponent (can be a vector)
# 
# Requirements:
# None
# 
# References
# Kalinkat et al. (under review): Empirical evidence of type III functional responses and why it remains rare.
# Okuyama & Ruyle (2011):Solutions for functional response experiments. Acta Oecologia 37: 512-516. https://doi.org/10.1016/j.actao.2011.07.002
# Real (1977): The kinetics of functional response. Am Nat 111: 289-300. https://doi.org/10.1086/283161
# Vucic-Pestic et al. (2010): Allometric functional response model: body masses constrain interaction strengths. J Anim Ecol: 79: 249-256. https://doi.org/10.1111/j.1365-2656.2009.01622.x
#

calc_gen_fr <- function(N = seq(0, 20, length=1000),
                        Fmax = 10,
                        N0 = 10/3,
                        theta = c(1, 1.25, 1.5, 2, 3)){
  
  out <- foreach::foreach(i = 1:length(theta), .combine = cbind) %do% {
    Fmax * N^theta[i] / (N0^theta[i] + N^theta[i])
  }
  
  colnames(out) <- paste("theta = ", theta, sep ="")
  
  return(as.data.frame(cbind(N, out)))
}


