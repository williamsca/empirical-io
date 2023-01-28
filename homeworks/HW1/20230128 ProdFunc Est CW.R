rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, haven, stargazer, plm)
# haven: read .dta files
# stargazer: create summary tables
# plm: panel estimation techniques

dt <- as.data.table(read_dta("HW1/GMdata.dta"))

dt[, yrsPresent := .N, by = .(index)]
dt[, d357 := (sic3 == 357)]

# Q1: Summary Statistics ----
stargazer(dt, summary = TRUE, type = "text", omit = c("index", "sic3", "yr"))

stargazer(dt[yrsPresent > 1], summary = TRUE, type = "text", omit = c("index", "sic3", "yr"))

stargazer(dt[yrsPresent == 4], summary = TRUE, type = "text", omit = c("index", "sic3", "yr"))

table(dt$yr)
table(dt$sic3)

# Q2: Total, between, within, and random effects estimators ----
fmla <- "ldsal ~ -1 + lemp + ldnpt + ldrst + as.factor(yr):d357" # as.factor(yr) + as.factor(index)

# Column (3) of Table 3
plm.ols <- plm(fmla, data = dt, model = c("pooling"), index = c("yr", "index"))
plm.with <- plm(fmla, data = dt, model = c("within"), index = c("yr", "index"))
plm.bet <- plm(fmla, data = dt, model = c("between"), index = c("yr", "index"))
# plm.re <- plm(fmla, data = dt, model = c("random"), index = c("yr", "index"))

stargazer(lm.ols, plm.with, plm.bet, type = "text", omit = c("yr", "d357", "index"),
          covariate.labels = c("Labor", "Physical Capital", "R&D Capital"),
          column.labels = c("Total", "Within",))

# These both yield column (2) of Table 3
lm.ols_bal <- lm(fmla, data = dt[yrsPresent == 4])
plm.with <- plm(fmla, data = dt[yrsPresent == 4], model = c("within"), index = c("yr", "index"))
stargazer(lm.ols_bal, plm.with, type = "text", omit = c("yr", "d357", "index"), 
          covariate.labels = c("Labor", "Physical Capital", "R&D Capital"))
          


plm.bet_bal <- plm(fmla, data = dt[yrsPresent == 4], model = c("within"), index = c("yr", "index"))
plm.ht_bal <- plm(fmla, data = dt[yrsPresent == 4], model = c("within"), index = c("yr", "index"))
plm.bet_bal <- plm(fmla, data = dt[yrsPresent == 4], model = c("within"), index = c("yr", "index"))

stargazer(plm.bet, plm.bet_bal, type = "text", omit = c("yr", "d357", "index"), 
          covariate.labels = c("Labor", "Physical Capital", "R&D Capital"))
