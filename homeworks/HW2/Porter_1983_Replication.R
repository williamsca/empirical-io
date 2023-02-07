# Replication files for Porter (1983): A study of cartel stability
# Author: Colin Williams
# Last updated: 7 Feb 2023

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, haven, stargazer, plm)
# haven: read .dta files
# stargazer: create summary tables
# plm: panel estimation techniques

dt <- as.data.table(read_dta("HW2/railway.dta"))
