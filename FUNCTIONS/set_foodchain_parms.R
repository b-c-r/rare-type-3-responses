################################################################################
#    Function "set_foodchain_parms" to set values for a 3-species food chain   #
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
# The function sets the parameter values for the compiled foodchain (chainC). 
# You need to execute compile_foodchain() first! See the description of
# compile_foodchain() for population model. See also the supplement of Kalinkat
# et al. (under review) for further methodology description.
#
# Arguments:
# a:  the allometric constant (default = 0.2227; see Otto et al. 2008)
# b:  the allometric exponent (default = -0.25; see Otto et al. 2008)
# e:  the assimilation efficiency (default = 0.85; see Otto et al. 2008)
# y:  the relative maximum feeding rate (default = 8; see Otto et al. 2008)
# N0: the half saturation rate (default = 0.5; see Otto et al. 2008)
# q:  the shaping exponent (default = 0; see Williams and Martinez 2004)
# R:  the body mass ratio (default = 100; see Otto et al. 2008)
# 
# Requirements:
# Compilation of the food chain model (run compile_foodchain())
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr)
# 
# References
# Kalinkat et al. (under review): Empirical evidence of type III functional responses and why it remains rare.
# Keitt (2007): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71, https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359.
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1.
#

set_foodchain_parms <- function(a = 0.2227,
                                b = -0.25,
                                e = 0.85,
                                y = 8,
                                N0 = .5,
                                q = 0,
                                R = 100){
  R2 <- R^2
  chainC_set_params(m1 = a * R^b,
                    m2 = a * R2^b,
                    e = e,
                    fmax1 = y * a * R^b,
                    fmax2 = y * a * R2^b,
                    N0 = N0,
                    q = q)
}



