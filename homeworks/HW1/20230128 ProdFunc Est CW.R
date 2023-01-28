# Replication files for Griliches and Mairesse (1995)
# Author: Colin Williams
# Last updated: 28 Jan 2023

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

dt[, yrsRange := max(yr) - min(yr), by = .(index)]
table(dt[, .(yrsPresent, yrsRange)]) # NOTE: the same firm may exit one year and reappear later

# Q1: Summary Statistics ----
stargazer(dt, summary = TRUE, type = "text", omit = c("index", "sic3", "yr"))

stargazer(dt[yrsPresent > 1], summary = TRUE, type = "text", omit = c("index", "sic3", "yr"))

stargazer(dt[yrsPresent == 4], summary = TRUE, type = "text", omit = c("index", "sic3", "yr"))

table(dt$yr)
table(dt$sic3)

# Q2: Total, between, within, and random effects estimators ----
fmla <- "ldsal ~ -1 + lemp + ldnpt + ldrst + as.factor(yr):d357" # as.factor(yr) + as.factor(index)

# * Full sample ----
plm.ols <- plm(fmla, data = dt2, model = "pooling", index = c("index", "yr")) # column (3) of Table 3
plm.with <- plm(fmla, data = dt2, model = "within", index = c("index", "yr"), effect = "twoway")
plm.bet <- plm(fmla, data = dt2, model = "between", index = c("index", "yr"))
plm.re <- plm(fmla, data = dt, model = "random", index = c("index", "yr"))

stargazer(plm.ols, plm.with, plm.bet, plm.re, type = "text", omit = c("yr", "d357", "index"),
          covariate.labels = c("Labor", "Physical Capital", "R&D Capital"),
          dep.var.labels = "Log sales",
          column.labels = c("Total OLS", "Within","Between", "Random"),
          title = "Production Function Parameters: Full Sample")

# * Balanced panel ----
plm.olsB <- plm(fmla, data = dt[yrsPresent == 4], model = "pooling", index = c("index", "yr")) # column (1) of Table 3
plm.withB <- plm(fmla, data = dt[yrsPresent == 4], model = "within", index = c("index", "yr"), effect = "twoway") # column (2) of Table 3
plm.betB <- plm(fmla, data = dt[yrsPresent == 4], model = "between", index = c("index", "yr"))
plm.reB <- plm(fmla, data = dt[yrsPresent == 4], model = "within", index = c("index", "yr"))

stargazer(plm.olsB, plm.withB, plm.betB, plm.reB, type = "text", omit = c("yr", "d357", "index"), 
          covariate.labels = c("Labor", "Physical Capital", "R&D Capital"), 
          dep.var.labels = "Log sales",
          column.labels = c("Total OLS", "Within","Between", "Random"),
          title = "Production Function Parameters: Balanced Panel")
          
# Q3: Hausman test of RE vs FE ----

# full sample
phtest(plm.with, plm.re) # p-value is significant at 1% --> reject RE model

# balanced panel
phtest(plm.withB, plm.reB) # p-value is insignificant at 5% --> fail to reject RE model


# Q4: Olley-Pakes estimator ----
# i) Predict sales with labor and a polynomial in the predetermined capital stock (ldnpt and ldrst) and investment (ldinv)
fmla.op1 <- "ldsal ~ -1 + lemp + as.factor(yr):d357 + poly(ldnpt, 2) + poly(ldrst, 2) + poly(ldinv, 2) + as.factor(yr)"
lm.op1 <- lm(fmla.op1, data = dt)
stargazer(lm.op1, type = "text", omit = c("d357", "poly"))

# ii) 
dt.shift <- copy(dt)

dt.shift[, lemp := shift(lemp, n = 1L, fill = NA, type = "lead"), by = .(index)]
dt.shift[, yr := shift(yr, n = 1L, type = "lead"), by = .(index)]
dt.shift[, d357 := shift(d357, n = 1L, type = "lead"), by = .(index)]

# compute predict sales given this period's employment and year FEs and last period's investment
dt.shift$predSal <- predict(lm.op1, dt.shift) 

dt <- merge(dt, dt.shift[, .(yr, index, predSal)], by = c("yr", "index"), all.x = TRUE)

dt[, LHS := ldsal - predSal]
dt[, ldnptDiff := ldnpt - shift(ldnpt, n = 1L, type = "lag"), by = .(index)]
dt[, ldrstDiff := ldrst - shift(ldrst, n = 1L, type = "lag"), by = .(index)]

fmla.op2 <- "LHS ~ -1 + ldnptDiff +  ldrstDiff + as.factor(yr) + as.factor(index)" # TODO: are these the correct FEs?
lm.op2 <- lm(fmla.op2, data = dt)
stargazer(lm.op2, type = "text", omit = c("yr", "index"))

# iii) Use probit model to estimate firm's survival probability
dt[, existsNextYr := (shift(yr, n = 1L, type = "lead") - yr == 5), by = .(index)]
dt[is.na(existsNextYr) & yr != 88, existsNextYr := FALSE]

glm.eNY <- glm(existsNextYr ~ ldnpt + ldrst + ldinv, family =  binomial(link = "probit"),
               data = dt)

dt$P <- predict(glm.eNY, dt)

# sanity checks on predicted probabilities of survival
summary(dt[existsNextYr == TRUE, P])




