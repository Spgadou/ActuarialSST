VaR.kernel <- function(alpha, data, Kernel, h = 0.01, lookback = length(data)){
  n <- length(data)
  vk <- 1:(n - lookback + 1); vk2 <- 1:lookback
  if (!is.numeric(try(Kernel(1:2), silent = TRUE))){
    Kernel <- Vectorize(Kernel)
  }
  sapply(vk, function(i){
    sortedR. <- sort(data[i:(i + lookback - 1)])
    sum(Kernel((vk2 / lookback - alpha) / h) * sortedR.) / (h * lookback)
  })
}
