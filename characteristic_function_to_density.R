characteristic_function_to_density <- function(
  phi, # characteristic function; should be vectorized
  n,   # Number of points, ideally a power of 2
  a, b # Evaluate the density on [a,b[
) {
  i <- 0:(n-1)            # Indices
  dx <- (b-a)/n           # Step size, for the density
  x <- a + i * dx         # Grid, for the density
  dt <- 2*pi / ( n * dx ) # Step size, frequency space
  c <- -n/2 * dt          # Evaluate the characteristic function on [c,d]
  d <-  n/2 * dt          # (center the interval on zero)
  t <- c + i * dt         # Grid, frequency space
  phi_t <- phi(t)
  X <- exp( -(0+1i) * i * dt * a ) * phi_t
  Y <- fft(X)
  density <- dt / (2*pi) * exp( - (0+1i) * c * x ) * Y
  data.frame(
    i = i,
    t = t,
    characteristic_function = phi_t,
    x = x,
    step = dx,
    density = Re(density)
  )
}