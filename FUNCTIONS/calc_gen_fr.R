################################################################################
#    Function "calc_gen_fr" to calculate functional response curves            #
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
# The function calculates the shapes of the generalized or theta-sigmoid
# functional response (e.g., Real 1977; Okuyama & Ruyle 2011; Vucic-Pestic et
# al. 2010). See Kalinkat et al. (2023) and Rall et al. (2023) for details.
#
# Arguments:
# N:     The resource densities (positive vector)
# Fmax:  The maximum feeding rate (the curves' asymptote, single value)
# N0:    The half saturation rate (density at which Fmax is reached, single value)
# Theta: The shaping exponent (can be a vector). Theta = 1 is a type II functional
#            response. Theta > 1 is a sigmoid type III functional response. Most
#            studies use Theta = 2 for modeling a type III functional response.
# 
# Requirements:
# None
# 
# References
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11: 1033818. https://doi.org/10.3389/fevo.2023.1033818
# Okuyama & Ruyle (2011):Solutions for functional response experiments. Acta Oecologia 37: 512-516. https://doi.org/10.1016/j.actao.2011.07.002
# Rall et al. (2023): Rare type III responses: Code & modelling methods (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# Real (1977): The kinetics of functional response. Am Nat 111: 289-300. https://doi.org/10.1086/283161
# Vucic-Pestic et al. (2010): Allometric functional response model: body masses constrain interaction strengths. J Anim Ecol: 79: 249-256. https://doi.org/10.1111/j.1365-2656.2009.01622.x
#

calc_gen_fr <- function(N = seq(0, 20, length=1000),
                        Fmax = 10,
                        N0 = 10/3,
                        Theta = c(1, 1.25, 1.5, 2, 3)){
  
  out <- foreach::foreach(i = 1:length(Theta), .combine = cbind) %do% {
    Fmax * N^Theta[i] / (N0^Theta[i] + N^Theta[i])                              # the functional response model
  }
  
  out <- as.data.frame(cbind(N, out))
  colnames(out) <- c("N", paste("Theta = ", Theta, sep =""))
  
  return(out)
}


