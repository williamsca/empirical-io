# Replication files 
# Author: Colin Williams
# Last updated: 25 Feb 2023

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, haven, knitr)

# Q1: Import airline dataset and aggregate to market level ----
dt <- as.data.table(read_dta("HW3/airline1997data.dta"))

dt <- unique(dt[tkcarrier %in% c("AA", "CO", "DL", "NW",  "TW", "UA", "US"), 
                .(market, tkcarrier, marketdistanceticket)])
dt[, nFirms := .N, by = .(market)]

dt.market <- unique(dt[, .(nFirms, market, marketdistanceticket)])

# Q2: Replicate Bresnahan and Reiss (1990, 1991) ----
Profits <- function(params, dist, nFirms) {
  alpha <- params[1]
  thetas <- params[2:8]
  
  return(dist*alpha + thetas[nFirms])
}

Prob <- function(params, dist, nFirms) {
  if (nFirms == 0) {
    return(pnorm(-Profits(params, dist, 1), log.p = TRUE))
  } else if (nFirms < 7) {
    return(log(pnorm(Profits(params, dist, nFirms)) - pnorm(Profits(params, dist, nFirms+1))))
  } else {
    return(pnorm(Profits(params, dist, nFirms), log.p = TRUE))
  }
}

Likelihood <- function(params, dist, nFirms) {
  dt.est[, f := Prob(params, marketdistanceticket, nFirms), by = seq_len(nrow(dt.est))]  
  return(-sum(dt.est$f))  
}

guess <- c(-.005, seq(-.1, -.7, -.1))
dt.est <- copy(dt.market)
optim <- optim(par = guess, Likelihood)

dt.optim <- data.table(Parameter = c("alpha", paste0("theta", 1:7)), Estimate = optim$par)
kable(dt.optim)
