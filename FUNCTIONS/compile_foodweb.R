################################################################################
#    Function "compile_foodweb" to compile a 10-species food web               #
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
# The function compiles the code for a 10-species food chain (Williams and
# Martinez, 2004) with some adaptations to the current question. The
# model will be compiled in C using odeintr (Keitt 2017). Please read the
# methodology in the supplement to the paper by Kalinkat et al. (2023), Rall et
# al. (2023) and Williams and Martinez (2004) for model details. Please consult
# the help file of odeintr (?odeintr::compile_sys) for the use of this specific
# function.
#
# Model parameters:
# y:  The relative maximum feeding rate of consumers.
# N0: The half saturation density.
# q:  The shaping exponent controlling the functional response shape
#     (0 = hyperbolic) and (0 > sigmoid).
# m:  The metabolic (loss) rate.
# e:  The assimilation efficiency.
# x:  The population biomass densities for the basal consumer (x[0]to x[9]).
#
# Arguments:
# none
# 
# Requirements:
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr)
# 
# References
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11: 1033818. https://doi.org/10.3389/fevo.2023.1033818
# Keitt (2017): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71; https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359
# Rall et al. (2023): Rare type III responses: methods for code and simulation models (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1
#

compile_foodweb <- function(){
  foodweb_code <- '
	dxdt[0] =
	  x[0] * (1 - x[0])
	- 0.5 * y * m2 * pow(x[0], (q+1)) / (pow(N0, (q+1)) + pow(x[0], (q+1))) * x[2]
	- 0.5 * y * m3 * pow(x[0], (q+1)) / (pow(N0, (q+1)) + pow(x[0], (q+1))) * x[3];
	
	dxdt[1] =
	  x[1] * (1 - x[1])
	- 0.5 * y * m2 * pow(x[1], (q+1)) / (pow(N0, (q+1)) + pow(x[1], (q+1))) * x[2]
	- 0.5 * y * m3 * pow(x[1], (q+1)) / (pow(N0, (q+1)) + pow(x[1], (q+1))) * x[3]
	- y * m4 * pow(x[1], (q+1)) / (pow(N0, (q+1)) + pow(x[1], (q+1))) * x[4];
	
	dxdt[2] = 
	  0.5 * e * y * m2 * pow(x[0], (q+1)) / (pow(N0, (q+1)) + pow(x[0], (q+1))) * x[2]
	+ 0.5 * e * y * m2 * pow(x[1], (q+1)) / (pow(N0, (q+1)) + pow(x[1], (q+1))) * x[2]
	- m2 * x[2]
	- 0.5 * y * m5 * pow(x[2], (q+1)) / (pow(N0, (q+1)) + pow(x[2], (q+1))) * x[5]
	- y * m6 * pow(x[2], (q+1)) / (pow(N0, (q+1)) + pow(x[2], (q+1))) * x[6];
	
	dxdt[3] = 
	  0.5 * e * y * m3 * pow(x[0], (q+1)) / (pow(N0, (q+1)) + pow(x[0], (q+1))) * x[3]
	+ 0.5 * e * y * m3 * pow(x[1], (q+1)) / (pow(N0, (q+1)) + pow(x[1], (q+1))) * x[3]
	- m3 * x[3]
	- 0.5 * y * m5 * pow(x[3], (q+1)) / (pow(N0, (q+1)) + pow(x[3], (q+1))) * x[5]
	- 0.4 * y * m7 * pow(x[3], (q+1)) / (pow(N0, (q+1)) + pow(x[3], (q+1))) * x[7];
	
	dxdt[4] = 
	  e * y * m4 * pow(x[1], (q+1)) / (pow(N0, (q+1)) + pow(x[1], (q+1))) * x[4]
	- m4 * x[4]
	- 0.4 * y * m7 * pow(x[4], (q+1)) / (pow(N0, (q+1)) + pow(x[4], (q+1))) * x[7]
	- 0.8 * y * m9 * pow(x[4], (q+1)) / (pow(N0, (q+1)) + pow(x[4], (q+1))) * x[9];
	
	dxdt[5] = 
	  0.5 * e * y * m5 * pow(x[2], (q+1)) / (pow(N0, (q+1)) + pow(x[2], (q+1))) * x[5]
  + 0.5 * e * y * m5 * pow(x[3], (q+1)) / (pow(N0, (q+1)) + pow(x[3], (q+1))) * x[5]
	- m5 * x[5]
	- 0.2 * y * m7 * pow(x[5], (q+1)) / (pow(N0, (q+1)) + pow(x[5], (q+1))) * x[7]
	- 0.2 * y * m9 * pow(x[5], (q+1)) / (pow(N0, (q+1)) + pow(x[5], (q+1))) * x[9];

	dxdt[6] = 
	  e * y * m6 * pow(x[2], (q+1)) / (pow(N0, (q+1)) + pow(x[2], (q+1))) * x[6]
	- m6 * x[6];
		
	dxdt[7] = 
	  0.4 * e * y * m7 * pow(x[3], (q+1)) / (pow(N0, (q+1)) + pow(x[3], (q+1))) * x[7]
	+ 0.4 * e * y * m7 * pow(x[4], (q+1)) / (pow(N0, (q+1)) + pow(x[4], (q+1))) * x[7]
	+ 0.2 * e * y * m7 * pow(x[5], (q+1)) / (pow(N0, (q+1)) + pow(x[5], (q+1))) * x[7]
	- m7 * x[7]
	- y * m8 * pow(x[7], (q+1)) / (pow(N0, (q+1)) + pow(x[7], (q+1))) * x[8];

	dxdt[8] = 
	  e * y * m8 * pow(x[7], (q+1)) / (pow(N0, (q+1)) + pow(x[7], (q+1))) * x[8]
	- m8 * x[8];

	dxdt[9] = 
	  0.8 * e * y * m9 * pow(x[4], (q+1)) / (pow(N0, (q+1)) + pow(x[4], (q+1))) * x[9]
	+ 0.2 * e * y * m9 * pow(x[5], (q+1)) / (pow(N0, (q+1)) + pow(x[5], (q+1))) * x[9]
	- m9 * x[9];
  '
  
  compile_sys("foodweb",
              foodweb_code,
              pars = c("m2",
                       "m3",
                       "m4",
                       "m5",
                       "m6",
                       "m7",
                       "m8",
                       "m9",
                       "e",
                       "y",
                       "N0",
                       "q"),
              method = "rk54_a",                                                # can compute adaptive time steps if required
              atol = 1e-12,                                                     # for more precise time series
              rtol = 1e-12)                                                     # for more precise time series
}


