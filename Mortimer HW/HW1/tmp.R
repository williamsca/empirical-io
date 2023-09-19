rm(list = ls())

library(data.table)
library(here)

# Import ----
dt <- fread(here("Mortimer HW", "data", "ps1_data_nohead.txt"))
names(dt) <- c("price", "quantity", "weight", "hp", "ac", "firmId")

M <- 100000000
lambda <- 4 * 10^(-6)

dt$share <- dt$quantity / M

setorder(dt, price)

# (1) Solve for delta ----
Q <- sum(dt$quantity)
S0 <- 1 - Q / M

deltas <- rep(NA, nrow(dt))

deltas[1] <- -dt$price[1] / lambda * log(S0)
deltas[2] <- deltas[1] - log(S0 + dt$share[1]) *
    (dt$price[2] - dt$price[1]) / lambda

for (i in seq(3, nrow(dt) - 1)) {
    deltas[i] <- deltas[i - 1] - (dt$price[i] - dt$price[i - 1]) *
        log(dt$share[i - 1] +
            exp(lambda * (deltas[i - 1] - deltas[i - 2]) /
            (dt$price[i - 1] - dt$price[i - 2]))
        ) /
        lambda
}
deltas[nrow(dt)] <- log(1 - dt$share[nrow(dt)]) / lambda *
    (dt$price[nrow(dt) - 1] - dt$price[nrow(dt)]) + deltas[nrow(dt) - 1]

dt$delta <- deltas

# (2) Estimate beta ----
# OLS
lm_beta <- lm(delta ~ weight + hp + ac, data = dt)

# GMM
Y <- as.matrix(dt$delta)
X <- as.matrix(dt[, .(1, weight, hp, ac)])

nMoms <- ncol(X)

b_start <- solve(t(X) %*% X) %*% t(X) %*% Y # starting beta value
W <- diag(nMoms) # weighting matrix

objGMM <- function(b, Y, X, W) {
    nObs = nrow(Y)
    vAvgMom = (1 / nObs) * (t(X)) %*% (Y - X %*% b)

    J = nObs * t(vAvgMom) %*% W %*% vAvgMom

    return(J)
}

#First step of GMM
b_hat_gmm <- optim(b_start, objGMM, Y = Y, X = X, W = W, method = "BFGS",
                   control = list(maxit = 1e5, reltol = 1e-12))
ep_hat_gmm <- Y - X %*% b_hat_gmm$par

#Get optimal weight matrix
s_hat <- solve((1 / nrow(Y)) * t(X) %*%
    diag(diag(ep_hat_gmm %*% t(ep_hat_gmm))) %*% X
)

#Second step of GMM
b_hat_gmm_e <- optim(b_hat_gmm$par, objGMM, Y = Y, X = X, W = s_hat,
                     method = "BFGS",
                     control = list(maxit = 10000, reltol = 1e-12))

#Find s^2
epsilon <- Y - X %*% b_hat_gmm_e$par
s_squared <- t(epsilon) %*% epsilon / (nrow(Y) - length(b_hat_gmm_e$par))

## Find Standard Errors
se_ols <- sqrt(diag(s_squared[1] * solve(t(X) %*% X)))

final <- as.data.frame(t(cbind(b_hat_gmm_e$par, se_ols)))
row.names(final) <- c("2 step GMM: ", "SE: ")
colnames(final) <- c("Constant", "weight", "hp", "ac")
final
