VaR.weighted <- function(alpha, data, lam = 0.99, lookback = length(data)){
  n <- length(data)
  vk <- 1:(n - lookback + 1)
  w <- sapply(1:lookback, function(i) lam^(lookback - i + 1)); w <- w / sum(w)
  sapply(vk, function(i){
    selectedR. <- data[i:(i + lookback - 1)]
    ranking <- order(selectedR.)
    sort(selectedR., decreasing = FALSE)[min(which(cumsum(w[ranking]) >= alpha))]
  })
}
