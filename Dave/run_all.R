## Info ------------------------------------------------------------------------
##
## Script name: Run All
##
## Purpose of script: To produce all plots
##
## Author: David Turner
##
## Date Created: 6 Feb 2022
##
## ## Source scripts  ----------------------------------------------------------
scripts <- c("Dave/UCB-DC_IMD_plots.R",
             "Dave/UCB-DC_Rural_Urban_plots.R",
             "Dave/UCB-DC_national_per_drugs_plots.R",
             "Dave/UCB-DC_GP_In_hand_plots.R"
             )
sapply(scripts,source)
