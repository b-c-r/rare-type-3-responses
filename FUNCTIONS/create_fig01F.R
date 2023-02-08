################################################################################
#    Function "create_fig01F" to create Figure 01F for Kalinkat et al. (2023)  #
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
#
# Description:
# The function creates Figure 1(F) from Kalinkat et al. (2023), see also Rall et
# al. (2023) for details.
#
# Arguments:
# Strong_in: The name of the 1st data file that should be used (default = "strong_interactions.csv")
# Weak_in: The name of the 2nd data file that should be used (default = "weak_interactions.csv")
# input_path:  the (sub-)path were the output files should be saved to (default = "SIM_OUT/")
# output_path: the (sub-)path were the output files should be saved to (default = "FIG_OUT/")
# save_output: Logical (T/F). Should the output be saved as pdf?
# 
# Requirements:
# Needs data from run_foodweb_sim_mt() and package "mgcv".
# 
# References
# Kalinkat et al. (2023): Empirical evidence of type III functional responses and why it remains rare. Front Ecol Evol 11: 1033818. https://doi.org/10.3389/fevo.2023.1033818
# Rall et al. (2023): Rare type III responses: Code & modelling methods (v1.0.0). Zenodo; https://doi.org/10.5281/zenodo.7619822
# Wood (2022): mgcv: mixed gam computation vehicle with automatic smoothness estimation (1.8-41). https://CRAN.R-project.org/package=mgcv
# 

create_fig01F <- function(Strong_in = "strong_interactions.csv",
                          Weak_in = "weak_interactions.csv",
                          input_path = "SIM_OUT/",
                          output_path = "FIG_OUT/",
                          save_output = T){
  
  library("mgcv")
  
  ## check if the output folder exists, if not create it:
  if(!dir.exists(output_path)){
    dir.create(output_path)
    warning("The output path does not exist - I'll create it for you.")
  }
  
  ## import the data
  Strong <- read.csv(paste(input_path, Strong_in, sep = ""))
  Weak <- read.csv(paste(input_path, Weak_in, sep = ""))
  
  ## analyse the data using the gam function
  mod01 <- gam(cbind(div, 10-div) ~ s(q), data = Strong, family = "binomial")
  mod02 <- gam(cbind(div, 10-div) ~ s(q), data = Weak, family = "binomial")
  
  ## create prediction curves
  qnew <- seq(min(c(Strong$q, Weak$q)), max(c(Strong$q, Weak$q)), length = 1000)
  pred01 <- predict(mod01, type = "response", se.fit=TRUE, newdata = list(q = qnew))
  pred02 <- predict(mod02, type = "response", se.fit=TRUE, newdata = list(q = qnew))
  
  ## The plot device
  if(save_output) {pdf(file = paste(output_path, "Figure01F.pdf", sep =""))}
  
  par(las=1)
  plot(Strong$q+1, Strong$div,
       pch = 16, cex = 0.2, col = "red",
       ylim = c(0, 12),
       xlab = expression(Theta),
       ylab = "Species Richness"
  )
  points(Weak$q+1, Weak$div, pch = 16, cex = 0.2, col = "blue")
  
  lines(qnew+1, as.numeric(pred01$fit)*10, col = "red")
  lines(qnew+1, as.numeric(pred01$fit+pred01$se.fit)*10, col = "red")
  lines(qnew+1, as.numeric(pred01$fit-pred01$se.fit)*10, col = "red")
  
  lines(qnew+1, as.numeric(pred02$fit)*10, col = "blue")
  lines(qnew+1, as.numeric(pred02$fit+pred02$se.fit)*10, col = "blue")
  lines(qnew+1, as.numeric(pred02$fit-pred02$se.fit)*10, col = "blue")
  
  legend("bottomright",
         legend = c("Strong Interactions",
                    "Weak Interactions"),
         col = c("blue",
                 "red"),
         pch = 16,
         lty = 1,
         cex = 0.75)
  
  mtext("(F)",
        line = -1.5,
        adj = 0.05)
  
  if(save_output) {dev.off()}
  
}



