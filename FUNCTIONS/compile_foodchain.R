################################################################################
#    Function "compile_foodchain" to compile a 2-species food chain            #
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
# The function compiles the code for a 3-species food chain (Otto et al. 2007)
# with additional shaping exponent q (e.g. Williams and Martinez 2004). The
# model will be compiled in C using odeintr (Keitt 2017). Please read the
# methodology in the supplement to the paper by Kalinkat et al. (under
# review) and Williams and Martinez for model details. Please consult the help
# file of odeintr (?odeintr::compile_sys) for the use of this specific function.
#
# Model parameters:
# fmax1 / fmax2: The maximum feeding rate of the intermediate consumer and the 
#                top consumer, respsectively.
# N0: The half saturation density.
# q: The shaping exponent controlling the functional response shape
#    (0 = hyperbolic) and (0 > sigmoid).
# m1 / m2: The metabolic (loss) rate.
# e: The assimilation efficiency.
# x: The population biomass densities for the basal consumer (x[0]), the
#    intermediate consumer (x[1]), and the top consumer (x[2]).
#
# Arguments:
# none
# 
# Requirements:
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr)
# 
# References
# Kalinkat et al. (under review): Empirical evidence of type III functional responses and why it remains rare.
# Keitt (2007): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71, https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359.
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1.
#

compile_foodchain <- function(){
  chain_code <- '
	dxdt[0] = x[0] * (1 - x[0]) - fmax1 * pow(x[0], (q+1)) / (pow(N0, (q+1)) + pow(x[0], (q+1))) * x[1];
	dxdt[1] = e * fmax1 * pow(x[0], (q+1)) / (pow(N0, (q+1)) + pow(x[0], (q+1))) * x[1] - m1 * x[1] - fmax2 * pow(x[1], (q+1)) / (pow(N0, (q+1)) + pow(x[1], (q+1))) * x[2];
	dxdt[2] = e * fmax2 * pow(x[1], (q+1)) / (pow(N0, (q+1)) + pow(x[1], (q+1))) * x[2] - m2 * x[2];
  '
  compile_sys("chainC",
              chain_code,
              pars = c("m1", "m2", "e", "fmax1", "fmax2", "N0", "q"),
              method = "rk54_a",
              atol = 1e-8,
              rtol = 1e-8)
}


