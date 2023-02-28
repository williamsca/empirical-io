# Replication files 
# Author: Colin Williams
# Last updated: 25 Feb 2023

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, haven, stargazer, ggplot2, fixest)

# Question 1: Download airline dataset ----
dt <- as.data.table(read_dta("HW3/airline1997data.dta"))

# NOTE: unique by origin, dest, tkcarrier, T100nonstop

dt[tkcarrier %in% c("AA", "CO", "DL", "NW", "TW", "UA", "US"), carrierType := "Legacy"]
dt[tkcarrier == "WN", carrierType :=  "Southwest"] 
dt[is.na(carrierType), carrierType := "Other LCC"]

# * Compute how many markets are served by an origin-carrier pair ----
dt[, nDest := uniqueN(dest), by = .(origin, tkcarrier)]
dt[, mktShare := totalpassengers / marketsize]
dt[, outsideShare := 1 - sum(mktShare), by = .(market)]


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
fmla <- "log(mktShare) ~ -1 + MdW_oneway_itinfare_ticket + marketdistanceticket + nDest + log(outsideShare)"
utility.ols <- lm(fmla, data = dt)
stargazer(utility.ols, type = "text")

ggplot(data = as.data.table(utility.ols$residuals), mapping = aes(x = V1)) +
  geom_histogram(bins = 90) +
  labs(x = "Residuals", y = "Frequency")

# TODO: compute the elasticity of demand for each good in each market?

# MC = P(1 + 1/e), but what about multi-product firms?

# OLS w/ FEs ----
fmla.fe <- paste0(fmla, " + tkcarrier")
utility.olsfe <- lm(fmla.fe, data = dt)
stargazer(utility.ols, utility.olsfe, type = "text", omit = c("tkcarrier"),
          add.lines = list(c("Airline FEs", "No", "Yes")),
          omit.stat = c("F"))

# TODO: compute the elasticity of demand for each good in each market?

ggplot(data = as.data.table(utility.olsfe$residuals), mapping = aes(x = V1)) +
  geom_histogram(bins = 90) +
  labs(x = "Residuals", y = "Frequency")

# Compute the instruments (BLP 1995, eqn (5.8)) ----
dt[, rivalNDest := sum(nDest) - nDest, by = .(market)]

fmla.fs <- "MdW_oneway_itinfare_ticket ~ -1 + nDest + rivalNDest"
iv.fs <- lm(fmla.fs, data = dt)
dt$p_hat <- predict(iv.fs, dt)

fmla.ss <- "log(mktShare) ~ -1 + p_hat + marketdistanceticket + log(outsideShare)"
iv.ss <- lm(fmla.ss, data = dt)
stargazer(utility.ols, iv.ss, type = "text")

ggplot(data = as.data.table(iv.ss$residuals), mapping = aes(x = V1)) +
  geom_histogram(bins = 90) +
  labs(x = "Residuals", y = "Frequency")


# Nested Logit ----
