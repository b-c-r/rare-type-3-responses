################################################################################
#    Function "create_fig01F" to create Figure 01 (F) from                     #
#        Kalinkat et al. (under review)                                        #
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
# The function creates Figure 1(F) from Kalinkat et al. (under review)
#
# Arguments:
# B_in:        Bifurcation data for the basal species. The *.CSV name (default = "bifout_basal.csv")
# I_in:        Bifurcation data for the intermediate species. The *.CSV name (default = "bifout_intermediate.csv")
# T_in:        Bifurcation data for the top species. The *.CSV name (default = "bifout_top.csv")
# input_path:  the (sub-)path were the output files should be saved to (default = "SIM_OUT/")
# output_path: the (sub-)path were the output files should be saved to (default = "FIG_OUT/")
# save_output: Logical (T/F). Should the output be saved as pdf?
# 
# The default settings are as used in Kalinkat et al. (under review).
# 
# Requirements:
# Needs bifurcation data, eventually simulated with run_foodchain_sim_mt()
# 
# References
# Kalinkat et al. (under review): Empirical evidence of type III functional responses and why it remains rare.
# 

create_fig01F <- function(Strong_in = "strong_interactions.csv",
                          Weak_in = "weak_interactions.csv",
                          input_path = "SIM_OUT/",
                          output_path = "FIG_OUT/",
                          save_output = T){
  
  library("mgcv")
  
  if(!dir.exists(output_path)){
    dir.create(output_path)
    warning("The output path does not exist - I'll create it for you.")
  }
  
  Strong <- read.csv(paste(input_path, Strong_in, sep = ""))
  Weak <- read.csv(paste(input_path, Weak_in, sep = ""))
  
  ## analyse the data
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



