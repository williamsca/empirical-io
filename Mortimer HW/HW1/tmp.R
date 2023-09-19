rm(list=ls())

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

