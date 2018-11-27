VaR.historical <- function(alpha, data, lookback = length(data)){
  n <- length(data)
  vk <- 1:(n - lookback + 1)
  sapply(vk, function(i){
    sort(data[i:(i + lookback - 1)])[ceiling(lookback * alpha)]
  })
}
