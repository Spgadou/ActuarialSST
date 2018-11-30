impVol <- function(price, S, K, r, q, tau, type = "call", bounds = c(0, 2)){
  opt <- function(sig){
    abs(BSPrice(S, K, tau, sig, r, q, type) - price)
  }
  optimize(opt, bounds)
}