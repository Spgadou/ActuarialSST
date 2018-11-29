########################
## Density derivation ##
########################
##
## Goal: Find the shape of density of ST,
##       given its dynamics
##
## Author: Simon-Pierre Gadoury
## Date: 11-29-2018

# Asset dynamics: dS = aS dt + b sqrt(S) dW
# a: constant
# b: constant
# (W): Brownian motion

a <- 0.05; b <- sqrt(0.15)
S0 <- 100; Mat <- 5

# Sub-goal (1): Find the characteristic function of ST given S0
# -> Use Feynman-Kac to find the characteristic function's ODEs.
# -> Solve the ODEs numerically with "deSolve".

f <- function(u){
  times <- c(0, Mat)
  parameters <- c("a" = a, "b" = b)
  state <- c("A" = 0, "B" = 1i * u)
  
  Feynmann <- function(t, state, parameters){
    with(as.list(c(state, parameters)),{
      dA <- 0
      dB <- B * a + 0.5 * b^2 * B^2
      list(c(dA, dB))
    })
  }
  
  out <- deSolve::zvode(y = state, times = times,
                        func = Feynmann, parms = parameters)
  
  return(out)
}

CF <- Vectorize(function(u){
  fit <- tail(f(u), 1)
  exp(fit[,'A'] + fit[,'B'] * S0) # 'Ansatz'
}, 'u')

# Sub-goal (2): Derive the density function from the CF
# -> We already coded such a function.

dirname <- "/Examples"
filename <- "/DensityDerivation_Feynman.R"
pathstring <- rstudioapi::getSourceEditorContext()$path
newPathstring <- substr(pathstring, 1, nchar(pathstring) - nchar(filename))
newPathstring <- substr(newPathstring, 1, nchar(newPathstring) - nchar(dirname))
newPathstring <- paste(newPathstring, "/characteristic_function_to_density.R",
                       sep = "")
source(newPathstring)

d <- characteristic_function_to_density(
  function(t) 
    CF(t),
  2^10,
  50, 200
)
plot(d$x, d$density, las=1, type = "l")
