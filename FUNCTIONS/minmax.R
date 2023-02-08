################################################################################
#    Function "minmax" to calculate all possible extrema in a time series      #
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
# The function returns the positions or values of all possible (local) extremes
# of a vector. This output goes beyond the min() and max() functions that only
# return the global minima and maxima. minmax() is comparable to the extrema()
# function from the package EMD (https://CRAN.R-project.org/package=EMD).
#
# Arguments:
# 
# x - A simple vector containing numerical values of a time series (e.g.
#     population densities).
# 
# return_vals - logical; if TRUE (default) the values of all extrema will be
#               returned; if FALSE, the positions will be returned.
#

minmax <- function(x, return_vals = T){
  # is the time series in equilibrium (i.e. no maxima)
  if(min(x) == max(x)){
    if(return_vals == T){
      return(mean(x))
    } else{
      return(length(x))
      warning("No maxima found, returning last positons")
    }
  } else{
    minpos <- which(x[c(-length(x), -(length(x)-1))] > x[c(-1,-length(x))] & x[c(-1,-length(x))] < x[c(-1,-2)])+1
    maxpos <- which(x[c(-length(x), -(length(x)-1))] < x[c(-1,-length(x))] & x[c(-1,-length(x))] > x[c(-1,-2)])+1
    if(return_vals == T){
      return(x[c(minpos,maxpos)])
    } else{
      return(list(min_position <- minpos,
                  max_position <- maxpos))
    }
  }
}
