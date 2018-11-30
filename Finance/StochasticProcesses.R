library(R6)

## 1 risk factor modeling
## 
## dS: function of (S, V), returning a vector (drift, vol)
## dV: function of (V), returning a vector (drift, vol)
## rho: correlation between stock and volatility

StochasticProcess1RF <- R6::R6Class("StochasticProcess1RF", list(
  S0 = NA,
  V0 = NA,
  dS = NA,
  dV = NA,
  rho = NA,
  paths = NA,
  initialize = function(S0, V0, dS, dV, rho){
    self$S0 <- S0; self$V0 <- V0
    self$dS <- dS; self$dV <- dV
    self$rho <- rho
  },
  simulate = function(m, mat, dt){
    n <- ceiling(mat / dt)
    paths <- list("S" = matrix(0, ncol = n, nrow = m),
                  "V" = matrix(0, ncol = n, nrow = m))
    for (j in 1:m){
      S. <- V. <- numeric(n)
      S.[1] <- self$S0; V.[1] <- self$V0
      Z.. <- mvtnorm::rmvnorm(n, sigma = rbind(c(1, self$rho),
                                               c(self$rho, 1)))
      for (i in 2:n){
        S.[i] <- S.[i - 1] + sum(self$dS(S.[i - 1], V.[i - 1]) *
                       c(dt, sqrt(dt) * Z..[i, 1]))
        V.[i] <- V.[i - 1] + sum(self$dV(V.[i - 1]) *
                       c(dt, sqrt(dt) * Z..[i, 2]))
      }
      paths[["S"]][j,] <- S.; paths[["V"]][j,] <- V.
    }
    self$paths <- paths
  }
))

## example:

heston <- StochasticProcess1RF$new(S0 = 100, V0 = 0.15, ## Initial values
                               dS = function(S, V){ ## Stock dynamics
                                 c(0.05 * S, S * sqrt(V))
                               },
                               dV = function(V){ ## Volatility dynamics
                                 c(0.2 * (0.15 - V), 0.2 * sqrt(V))
                               },
                               rho = -0.6)

m <- 5
res <- heston$simulate(m = m, mat = 5, dt = 1/250) ## Simulation
par(mfrow = c(2,1))
plot(res$S[1,], type = "l", ylim = c(0, 600),
     main = "Stock",
     ylab = expression(S(omega)))
for (i in 2:m){
  points(x = 1:length(res$S[1,]),
         y = res$S[i,], col = i, type = "l")
}
plot(res$V[1,], type = "l", ylim = c(0, 0.50),
     main = "Volatility",
     ylab = expression(V(omega)))
for (i in 2:m){
  points(x = 1:length(res$S[1,]),
         y = res$V[i,], col = i, type = "l")
}





