rm(list=ls())

library(data.table)
library(here)

# Import ----
dt <- fread(here("Mortimer HW", "data", "ps1_data_nohead.txt"))
names(dt) <- c("price", "quantity", "weight", "hp", "ac", "firmId")

M <- 100000000
lambda <- 4 * 10^(-6)

setorder(dt, price)

# (1) Solve for delta ----
Q <- sum(dt$quantity)
S0 <- -exp(-lambda * )

View(dt)
