################################################################################
#    Function "set_foodchain_parms" to set values for a 3-species food chain   #
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
# The function sets the parameter values for the compiled foodchain. See the
# description of the odeintr package (Keitt 2017) for details how to apply
# such functions. You need to execute compile_foodchain() first! See also the 
# description in compile_foodchain.R for the model. See Kalinkat et al. (2023),
# Rall et al. (2023), and references below for further description.
#
# Arguments:
# a:  the allometric constant (default = 0.2227; see Otto et al. 2007)
# b:  the allometric exponent (default = -0.25; see Otto et al. 2007)
# e:  the assimilation efficiency (default = 0.85; see Otto et al. 2007)
# y:  the relative maximum feeding rate (default = 8; see Otto et al. 2007)
# N0: the half saturation rate (default = 0.5; see Otto et al. 2007)
# q:  the shaping exponent (default = 0; see Williams and Martinez 2004)
# R:  the body mass ratio (default = 100; see Otto et al. 2007)
# 
# Requirements:
# Compilation of the food chain model (run compile_foodchain())
# Needs the package odeintr (https://CRAN.R-project.org/package=odeintr)
# 
# References
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11. https://doi.org/10.3389/fevo.2023.1033818
# Keitt (2017): odeintr: C++ ODE Solvers Compiled on-Demand, 1.71; https://CRAN.R-project.org/package=odeintr
# Otto et al. (2007): Allometric degree distributions facilitate food-web stability. Nature 450: 1226-1229; https://doi.org/10.1038/nature06359.
# Rall et al. (2023): Rare type III responses: Code & modelling methods (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# Williams and Martinez (2004): Stabilization of chaotic and non-permanent food-web dynamics. Eur Phys J B 38: 297-303; https://doi.org10.1140/epjb/e2004-00122-1.
#

set_foodchain_parms <- function(a = 0.2227,
                                b = -0.25,
                                e = 0.85,
                                y = 8,
                                N0 = .5,
                                q = 0,
                                R = 100){
  R2 <- R^2                                                                     ## calculation of the body mass ratio of the top to the basal species (see Otto et al. 2007)
  chainC_set_params(m1 = a * R^b,                                               ## calculation of the metabolic rate for the intermediate species (see Rall et al. 2023)
                    m2 = a * R2^b,                                              ## calculation of the metabolic rate for the top species (see Rall et al. 2023)
                    e = e,
                    fmax1 = y * a * R^b,                                        ## calculation of the maximum feeding rate for the intermediate species (see Rall et al. 2023)
                    fmax2 = y * a * R2^b,                                       ## calculation of the maximum feeding rate for the top species (see Rall et al. 2023)
                    N0 = N0,
                    q = q)
}



