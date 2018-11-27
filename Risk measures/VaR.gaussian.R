VaR.gaussian <- function(alpha, data, lam = 0.99, lookback = length(data)){
  n <- length(data)
  vk <- 1:(n - lookback + 1)
  w <- sapply(1:lookback, function(i) lam^(lookback - i)); w <- w / sum(w)
  sapply(vk, function(i){
    selectedR. <- data[i:(i + lookback - 1)]
    mu <- mean(selectedR.)
    sigma <- sqrt(sum(w * (selectedR. - mu)^2))
    mu + sigma * qnorm(alpha)
  })
}
