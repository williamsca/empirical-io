# Replication files for Porter (1983): A study of cartel stability
# Author: Colin Williams
# Last updated: 7 Feb 2023

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, haven, stargazer, stats4)
# haven: read .dta files
# stargazer: create summary tables
# stats4: maximum-likelihood estimation

dt <- as.data.table(read_dta("HW2/railway.dta"))
setcolorder(dt, neworder = c("week", "month", "gr", "tgq", "lakes", "po"))

dt[, month4 := ((floor((week - 1) / 4))) %% 13 + 1]
dt[, year := 1880 + floor((week - 1) / 52)]
dt[ , weekOfYear := seq_len(.N), by = year]

dt[, DM1 := fifelse(inrange(week, 28, 52*3 + 10), 1, 0)]
dt[, DM2 := fifelse(inrange(week, 167, 181), 1, 0)]
dt[, DM3 := fifelse(inrange(week, 182, 323), 1, 0)]
dt[, DM4 := fifelse(week >= 324, 1, 0)]

dt[, `:=`(lngr = log(gr), lntgq = log(tgq))]

# Table 2: Summary Statistics ----

stargazer(dt, summary  = TRUE, nobs = FALSE, type = "text", keep = c("gr", "tgq", "lakes", "po"))

# Table 3: Estimation Results ----

# * Columns (1) and (2): 2SLS ----
lm.FSPrice <- lm(lngr ~ lakes + DM1 + DM2 + DM3 + DM4 + po + as.factor(month4),  data = dt)
lm.FSQuant <- lm(lntgq ~ lakes + DM1 + DM2 + DM3 + DM4 + po + as.factor(month4),  data = dt)

dt$grHat <- fitted(lm.FSPrice)
dt$tgqHat <- fitted(lm.FSQuant)

lm.Demand <- lm(lntgq ~ grHat + lakes + as.factor(month4), data = dt)
lm.Supply <- lm(lngr ~ tgqHat + DM1 + DM2 + DM3 + DM4 + po + as.factor(month4), data = dt)

stargazer(lm.Demand, lm.Supply, type = "text", omit = "month4")

# * Columns (3) and (4): ML ----
h_density <- function(params, lntgq, lngr, lakes, DM1, DM2, DM3, DM4, It) {
  alpha0 <- params[1]
  alpha1 <- params[2]
  alpha2 <- params[3]
  beta0 <- params[4]
  beta1 <- params[5]
  beta21 <- params[6]
  beta22 <- params[7]
  beta23 <- params[8]
  beta24 <- params[9]
  beta3 <- params[10]
  sigma11 <- params[11]
  sigma12 <- params[12]
  sigma22 <- params[13]
  
  yt <- matrix(c(lntgq, lngr), nrow = 2)
  Xt <- matrix(c(1, lakes, DM1, DM2, DM3, DM4), nrow = 6, ncol = 1)  
  
  B <- matrix(c(1, -beta1, -alpha1, 1), nrow = 2, ncol = 2)
  Delta <- matrix(c(0, beta3), nrow = 2)
  Gamma <- matrix(c(alpha0, beta0, alpha2, 0, 0, beta21, 0, beta22, 0, beta23, 0, beta24), nrow = 2)
  Sigma <- matrix(c(sigma11, sigma12, sigma12, sigma22), nrow = 2)
  SigmaInv <- solve(Sigma)  
  Omega <- (B%*%yt-Gamma%*%Xt-Delta*It)

  return(log((2*pi)^(-1)*det(Sigma)^(-1/2)*det(B)*exp(t((-1/2)*Omega) %*% SigmaInv %*% Omega)))
}

h_Likelihood <- function(params) { 
  dt[, h := h_density(params, lntgq, lngr, lakes, DM1, DM2, DM3, DM4, It), by = seq_len(nrow(dt))]  
  return(-sum(dt$h))
}

dt[, It := po]

guess <- c(9, -1, -1, -3, 0, 0, 0, 0, 0, .5, .5, 0, .5)
guess2 <- c(0,0,0,0,0,0,0,0,0,0,.5,0,.5)
guess3 <- c(rep(10, 10), .5, 0, .5)
optim.po <- optim(par = guess, h_Likelihood)
optim.po2 <- optim(par = guess2, h_Likelihood)

dt.po <- data.table(param = c("alpha0", "alpha1", "alpha2", "beta0", "beta1", "beta21", 
                      "beta22", "beta23", "beta24", "beta3", "sigma11", "sigma12", "sigma22"), 
                    value = optim.po$par, value2 = optim.po2$par,
                    `2sls` = c(coefficients(lm.Demand)[1:3], coefficients(lm.Supply)[1:7], 1, 0, 1))

# * Columns (3) and (4): Better ML (if time) ----
Log_Likelihood <- function(params) {
  alpha0 <- params[1]
  alpha1 <- params[2]
  alpha2 <- params[3]
  beta0 <- params[4]
  beta1 <- params[5]
  beta21 <- params[6]
  beta22 <- params[7]
  beta23 <- params[8]
  beta24 <- params[9]
  beta3 <- params[10]
  sigma11 <- params[11]
  sigma12 <- params[12]
  sigma22 <- params[13]
  
  
}


# * Columns (3) and (4): Kiefer's algorithm ----
f_density <- function(params, lntgq, lngr, lakes, DM1, DM2, DM3, DM4, l) {
  lambda <- l
  return(lambda * h_density(params, lntgq, lngr, lakes, DM1, DM2, DM3, DM4, 1) +
         (1-lambda) * h_density(params, lntgq, lngr, lakes, DM1, DM2, DM3, DM4, 0))
}

f_Likelihood <- function(params, l) { 
  dt[, f := f_density(params, lntgq, lngr, lakes, DM1, DM2, DM3, DM4, l), by = seq_len(nrow(dt))]  
  return(-sum(dt$f))
}

dt[, w0 := po] # initial guess of regime classification

for (i in 1:10) { # does not converge :/
  lambda0 <- sum(dt$w0/nrow(dt)) # initial estimate of lambda
  
  optim.It <- optim(par = guess, f_Likelihood, l = lambda0)$par
  
  dt[, h1 := h_density(optim.It, lntgq, lngr, lakes, DM1, DM2, DM3, DM4, 1), by = seq_len(nrow(dt))]
  dt[, h0 := h_density(optim.It, lntgq, lngr, lakes, DM1, DM2, DM3, DM4, 0), by = seq_len(nrow(dt))]
  dt[, w1 := (lambda0*h1) / (lambda0*h1 + (1-lambda0)*h0)]
  
  print(cov(dt$w0, dt$w1))
  
  dt[, w0 := w1]
}








