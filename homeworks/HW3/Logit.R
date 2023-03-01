# Replication files 
# Author: Colin Williams
# Last updated: 25 Feb 2023

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, haven, stargazer, ggplot2, antitrust)
# antitrust: nested logit demand estimation and merger simulation

# Question 1: Download airline dataset ----
dt <- as.data.table(read_dta("HW3/airline1997data.dta"))

dt[tkcarrier %in% c("AA", "CO", "DL", "NW", "TW", "UA", "US"), carrierType := "Legacy"]
dt[tkcarrier == "WN", carrierType :=  "Southwest"] 
dt[is.na(carrierType), carrierType := "Other LCC"]

# Compute how many markets are served by an origin-carrier pair
dt[, nDest := uniqueN(dest), by = .(origin, tkcarrier)]
dt[, rivalNDest := sum(nDest) - nDest, by = .(market)] # for IV - see BLP 1995, eqn (5.8)

# Compute market shares
dt[, prodMktShare := totalpassengers / marketsize] # product market share
dt[, outsideShare := 1 - sum(prodMktShare), by = .(market)] # share of the outside option
dt[, firmMktShare := sum(prodMktShare), by = .(market, tkcarrier)] # account for multi-product firms
dt[, groupShare := totalpassengers / sum(totalpassengers), by  = .(market)] # within-group share


# Question 3: Summary statistics for prices and quantities ----
stargazer(dt, summary = TRUE, keep =  c("MdW_oneway_itinfare_ticket", "totalpassengers"),
          type = "text", summary.stat = c("n", "mean", "median", "sd", "min", "max"))

ggplot(dt, aes(x = MdW_oneway_itinfare_ticket)) + 
  geom_histogram() +
  labs(title = "", x = "Price ($)", y = "Frequency", caption = "") +
  theme_light() + theme(plot.caption = element_text(hjust = 0)) # left-align caption

ggplot(dt, aes(x = totalpassengers)) + 
  geom_histogram() +
  labs(title = "", x = "Total Passengers", y = "Frequency", caption = "") +
  theme_light() + theme(plot.caption = element_text(hjust = 0)) # left-align caption

ggplot(dt, aes(x = marketdistanceticket, y  = MdW_oneway_itinfare_ticket)) +
  geom_point(size = 1, alpha = .5) +
  geom_line(stat = "smooth", method = "loess", color = "red", size = 1) +
  labs(x = "Nonstop Distance (mi.)",  y = "Ticket Fare ($)") +
  theme_light()

lm.distance <- lm(MdW_oneway_itinfare_ticket ~ marketdistanceticket, data = dt)
stargazer(lm.distance, type = "text")

# Question 4: Summary statistics by firm type ----
stargazer(dt[carrierType == "Legacy"], summary = TRUE, keep =  c("MdW_oneway_itinfare_ticket", "totalpassengers"),
          type = "text", summary.stat = c("n", "mean", "median", "sd", "min", "max"))

stargazer(dt[carrierType == "Southwest"], summary = TRUE, keep =  c("MdW_oneway_itinfare_ticket", "totalpassengers"),
          type = "text", summary.stat = c("n", "mean", "median", "sd", "min", "max"))

stargazer(dt[carrierType == "Other LCC"], summary = TRUE, keep =  c("MdW_oneway_itinfare_ticket", "totalpassengers"),
          type = "text", summary.stat = c("n", "mean", "median", "sd", "min", "max"))

# Simple OLS ----
fmla <- "log(prodMktShare) ~ -1 + MdW_oneway_itinfare_ticket + marketdistanceticket + nDest + log(outsideShare)"
utility.ols <- lm(fmla, data = dt)
stargazer(utility.ols, type = "text")

# * Residual plot
ggplot(data = as.data.table(utility.ols$residuals), mapping = aes(x = V1)) +
  geom_histogram(bins = 90) +
  labs(x = "Residuals", y = "Frequency")

alpha.ols <- coefficients(utility.ols)["MdW_oneway_itinfare_ticket"]

# * Demand elasticities and markups ----
dt[, e.ols := -alpha.ols*MdW_oneway_itinfare_ticket*(1-firmMktShare)]
dt[, mc.ols := MdW_oneway_itinfare_ticket + 1 / (alpha.ols*(1-firmMktShare))]
dt[, markup.ols := (MdW_oneway_itinfare_ticket - mc.ols)/MdW_oneway_itinfare_ticket]

ggplot(data = dt, mapping = aes(x = e.ols)) +
  geom_histogram(bins = 50) +
  scale_x_continuous() +
  labs(x = "Demand Elasticity", y = "Frequency") +
  theme_light()

ggplot(data = dt, mapping = aes(x = markup.ols)) +
  geom_histogram(bins = 50) +
  scale_x_continuous() +
  labs(x = "Lerner Index", y = "Frequency") +
  theme_light()

# OLS w/ FEs ----
fmla.fe <- paste0(fmla, " + tkcarrier")
utility.olsfe <- lm(fmla.fe, data = dt)
stargazer(utility.ols, utility.olsfe, type = "text", omit = c("tkcarrier"),
          add.lines = list(c("Airline FEs", "No", "Yes")),
          omit.stat = c("F"))
alpha.fe <- coefficients(utility.olsfe)["MdW_oneway_itinfare_ticket"]

# * Residual plot
ggplot(data = as.data.table(utility.olsfe$residuals), mapping = aes(x = V1)) +
  geom_histogram(bins = 90) +
  labs(x = "Residuals", y = "Frequency")

# * Demand elasticities and markups ----
dt[, e.fe := -alpha.fe*MdW_oneway_itinfare_ticket*(1-firmMktShare)]
dt[, mc.fe := MdW_oneway_itinfare_ticket + 1 / (alpha.fe*(1-firmMktShare))]
dt[, markup.fe := (MdW_oneway_itinfare_ticket - mc.fe)/MdW_oneway_itinfare_ticket]

ggplot(data = dt, mapping = aes(x = e.fe)) +
  geom_histogram(bins = 50) +
  scale_x_continuous() +
  labs(x = "Demand Elasticity", y = "Frequency", title = "OLS with Fixed Effects") +
  theme_light()

ggplot(data = dt, mapping = aes(x = markup.fe)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(limits = c(0, 11)) +
  labs(x = "Lerner Index", y = "Frequency", title = "OLS with Fixed Effects") +
  theme_light()

# IV without FEs ----
fmla.fs <- "MdW_oneway_itinfare_ticket ~ -1 + nDest + rivalNDest"
iv.fs <- lm(fmla.fs, data = dt)
dt$p_hat <- predict(iv.fs, dt)

fmla.ss <- "log(prodMktShare) ~ -1 + p_hat + marketdistanceticket + log(outsideShare)"
iv.ss <- lm(fmla.ss, data = dt)
stargazer(utility.ols, iv.ss, type = "text")
alpha.iv <- coefficients(iv.ss)["p_hat"]

# * Residual plot
ggplot(data = as.data.table(iv.ss$residuals), mapping = aes(x = V1)) +
  geom_histogram(bins = 90) +
  labs(x = "Residuals", y = "Frequency")

# * Demand elasticities and markups ----
dt[, e.iv := -alpha.iv*MdW_oneway_itinfare_ticket*(1-firmMktShare)]
dt[, mc.iv := MdW_oneway_itinfare_ticket + 1 / (alpha.iv*(1-firmMktShare))]
dt[, markup.iv := (MdW_oneway_itinfare_ticket - mc.iv)/MdW_oneway_itinfare_ticket]

ggplot(data = dt, mapping = aes(x = e.iv)) +
  geom_histogram(bins = 50) +
  scale_x_continuous() +
  labs(x = "Demand Elasticity", y = "Frequency", title = "IV") +
  theme_light()

ggplot(data = dt, mapping = aes(x = markup.iv)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(limits = c(0, .75)) +
  labs(x = "Lerner Index", y = "Frequency", title = "IV") +
  theme_light()

# Nested Logit ----
fmla.nl <- "log(prodMktShare) ~ -1 + p_hat + marketdistanceticket + log(outsideShare) + log(groupShare)"
iv.nl <- lm(fmla.nl, data = dt)
stargazer(utility.ols, iv.nl, type = "text")
alpha.nl <- coefficients(iv.nl)["p_hat"]

# * Residual plot
ggplot(data = as.data.table(iv.nl$residuals), mapping = aes(x = V1)) +
  geom_histogram(bins = 90) +
  labs(x = "Residuals", y = "Frequency")

# * Demand elasticities and markups ----
dt[, e.nl := -alpha.nl*MdW_oneway_itinfare_ticket*(1-firmMktShare)]
dt[, mc.nl := MdW_oneway_itinfare_ticket + 1 / (alpha.nl*(1-firmMktShare))]
dt[, markup.nl := (MdW_oneway_itinfare_ticket - mc.nl)/MdW_oneway_itinfare_ticket]

ggplot(data = dt, mapping = aes(x = e.nl)) +
  geom_histogram(bins = 50) +
  scale_x_continuous() +
  labs(x = "Demand Elasticity", y = "Frequency", title = "Nested Logit") +
  theme_light()

ggplot(data = dt, mapping = aes(x = markup.iv)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(limits = c(0, .75)) +
  labs(x = "Lerner Index", y = "Frequency", title = "Nested Logit") +
  theme_light()

# Multi-nested Logit & Merger Simulations ----
v.fields <- c("market", "origin", "dest", "tkcarrier", "T100nonstop", "marketdistanceticket", 
              "totalpassengers", "MdW_oneway_itinfare_ticket", "marketsize", "carrierType", 
              "nDest")
write_dta(dt[, ..v.fields], "HW3/airline1997dataMERGERSIM.dta")


