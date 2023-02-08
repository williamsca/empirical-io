# Replication files for Porter (1983): A study of cartel stability
# Author: Colin Williams
# Last updated: 7 Feb 2023

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, haven, stargazer, plm)
# haven: read .dta files
# stargazer: create summary tables

dt <- as.data.table(read_dta("HW2/railway.dta"))
setcolorder(dt, neworder = c("week", "month", "gr", "tgq", "lakes", "po"))

# Table 2: Summary Statistics ----

stargazer(dt, summary  = TRUE, nobs = FALSE, type = "text", keep = c("gr", "tgq", "lakes", "po"))


# Table 3: Estimation Results ----

dt[, month4 := ((floor((week - 1) / 4))) %% 13 + 1]

# TODO: define DM1, ..., DM4

# TODO: ask Federico about Q_t, p_t?