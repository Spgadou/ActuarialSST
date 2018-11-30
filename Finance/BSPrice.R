BSPrice <- function(S, K, tau, sig, r, q, type = "call"){
  stopifnot(sum(sapply(c(S, K, tau, sig, r, q), is.numeric)) == 6)
  d1 <- (log(S / K) + (r - q + 0.5 * sig^2) * tau) / (sig * sqrt(tau))
  d2 <- d1 - sig * sqrt(tau)
  if (type == "call"){
    S * exp(-q * tau) * pnorm(d1) - K * exp(-r * tau) * pnorm(d2)
  } else if (type == "put"){
    K * exp(-r * tau) * pnorm(-d2) - S * exp(-q * tau) * pnorm(-d1)
  } else{
    stop("Wrong 'type' entry.")
  }
}