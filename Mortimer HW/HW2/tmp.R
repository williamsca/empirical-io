rm(list = ls())
library(here)
library(data.table)

# Import ----
dt <- fread(here("Mortimer HW", "data", "ps2_data_3nests.txt"))
setnames(dt, c("car_id", "year", "firm_id", "price", "quantity",
               "weight", "hp", "ac", "nest3"))

M <- 100 * 10^6 # market size

# Normalize weight, horsepower, and price by dividing by their
# respective means
v_norm <- c("weight", "hp", "price")
dt[, (v_norm) := lapply(.SD, function(x) x / mean(x)), .SDcols = v_norm]

dt[, const := 1]
dt[, share := quantity / M]

# Construct BLP instruments for price using the characteristics
# of competing products within each year
v_char <- c("weight", "hp", "ac")
dt[, paste0("z_", v_char) := lapply(.SD,
    function(x) sum(x) - x), by = year, .SDcols = v_char]

# Estimate demand paramters by GMM
v_instr <- paste0("z_", v_char)
v_cov <- c("price", v_char, "const")
Y <- as.matrix(dt$share)
X <- as.matrix(dt[, ..v_cov])
Z <- as.matrix(dt[, ..v_instr])

n_moments <- ncol(Z)

# starting beta value
b_start <- solve(t(X) %*% X) %*% t(X) %*% Y

W <- diag(n_moments) # weighting matrix

objGMM <- function(b, W, X, Y, Z) {
    nObs <- nrow(Y)
    vAvgMom <- (1 / nObs) * (t(Z)) %*% (Y - X %*% b)

    return(nObs * t(vAvgMom) %*% W %*% vAvgMom)
}

# First step of GMM
b_hat_gmm <- optim(b_start, objGMM,
    W = W, X = X, Y = Y, Z = Z, method = "BFGS",
    control = list(maxit = 1e5, reltol = 1e-12)
)
ep_hat_gmm <- Y - X %*% b_hat_gmm$par

# Get optimal weight matrix
s_hat <- solve((1 / nrow(Y)) * t(Z) %*%
    diag(diag(ep_hat_gmm %*% t(ep_hat_gmm))) %*% Z)

# Second step of GMM
b_hat_gmm_e <- optim(b_hat_gmm$par, objGMM,
    W = s_hat, X = X, Y = Y, Z = Z, 
    method = "BFGS",
    control = list(maxit = 10000, reltol = 1e-12)
)

# Find s^2
epsilon <- Y - X %*% b_hat_gmm_e$par
s_squared <- t(epsilon) %*% epsilon / (nrow(Y) - length(b_hat_gmm_e$par))

## Find Standard Errors
se_ols <- sqrt(diag(s_squared[1] * solve(t(X) %*% X)))

final <- as.data.frame(t(cbind(b_hat_gmm_e$par, se_ols)))
row.names(final) <- c("2 step GMM: ", "SE: ")
colnames(final) <- v_cov
final

## Cross-Price Elasticites
setorder(dt, -quantity)

dt_top10 <- head(dt[year == 1990], 10)
alpha <- b_hat_gmm_e$par[1]

# Compute the matrix of elasticities
dt_elas <- CJ(
    own_product = dt_top10$car_id,
    cross_product = dt_top10$car_id
)

dt_elas <- merge(dt_elas, dt_top10[, .(car_id, price, share)],
                 by.x = "own_product", by.y = "car_id")
dt_elas <- merge(dt_elas, dt_top10[, .(car_id, price, share)],
                 by.x = "cross_product", by.y = "car_id")
dt_elas[own_product == cross_product,
    price_elas := alpha * price.x * (share.x - 1)]
dt_elas[own_product != cross_product, price_elas := alpha * price.y * share.y]

dt_elas <- dcast(dt_elas, own_product ~ cross_product, value.var = "price_elas")

# Reshape the data to wide format to get the matrix
elasticity_matrix_wide <- dcast(elasticity_matrix, own_product ~ cross_product,
                                value.var = "elasticity")

print(elasticity_matrix_wide)
