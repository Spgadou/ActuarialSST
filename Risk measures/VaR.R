# install.packages("R6)
# install.packages("rstudioapi")

## Load VaR functions

if (sys.nframe() == 0L){
  pathstring <- rstudioapi::getSourceEditorContext()$path
  newPathstring <- substr(pathstring, 1, nchar(pathstring) - 5)
  files <- list.files(newPathstring)
  files <- files[files != "VaR.R"]
  files <- files[substr(files, 1, 3) == "VaR"]
  paths <- sapply(files, function(x) paste(newPathstring, x, sep = ""))
  invisible(sapply(paths, source))
} else {
  newPathstring <- getwd()
  files <- list.files(newPathstring)
  files <- files[files != "VaR.R"]
  files <- files[substr(files, 1, 3) == "VaR"]
  invisible(sapply(files, source))
}


## Master VaR class
VaR <- R6::R6Class("VaR",list(
  Var = list(),
  R. = 0,
  t. = 0,
  types = substr(files, 5, nchar(files) - 2),
  initialize = function(data, timeIndex){
    self$R. <- data
    self$t. <- timeIndex
  },
  Plot = function(){
    k <- 2
    plot(x = self$t., y = self$R., type = "l", col = "lightgray",
         xlab = "time", ylab = expression(VaR[alpha]),
         main = "VaR time-series")
    for (element in self$Var){
      points(x = element$time,
             y = element$value, type = "l", col = k)
      k <- k + 1
    }
    if (length(self$Var) != 0)
      legend("topleft", legend = names(self$Var),
             col = 2:(length(self$Var) + 1), lty = rep(1, length(self$Var)), cex = 0.6,
             bty = "n",  y.intersp = 0.5)
  },
  Delete = function(name){
    self$Val[[name]] <- NULL
  },
  historical = function(...){
    Value <- VaR.historical(data = self$R., ...)
    lookback <- length(self$R.) - length(Value) + 1
    Time <- c(self$t.[(lookback + 1):length(self$R.)], self$t.[length(self$R.)] + 1)
    self$Var[["historical"]] <- data.frame("time" = Time,
                                           "value" = Value)
  },
  weighted = function(...){
    Value <- VaR.weighted(data = self$R., ...)
    lookback <- length(self$R.) - length(Value) + 1
    Time <- c(self$t.[(lookback + 1):length(self$R.)], self$t.[length(self$R.)] + 1)
    self$Var[["weighted"]] <- data.frame("time" = Time,
                                         "value" = Value)
  },
  kernel = function(...){
    Value <- VaR.kernel(data = self$R., ...)
    lookback <- length(self$R.) - length(Value) + 1
    Time <- c(self$t.[(lookback + 1):length(self$R.)], self$t.[length(self$R.)] + 1)
    self$Var[["kernel"]] <- data.frame("time" = Time,
                                       "value" = Value)
  },
  gaussian = function(out = FALSE, ...){
    Value <- VaR.gaussian(data = self$R., ...)
    if (out){
      return(Value)
    }
    lookback <- length(self$R.) - length(Value) + 1
    Time <- c(self$t.[(lookback + 1):length(self$R.)], self$t.[length(self$R.)] + 1)
    self$Var[["gaussian"]] <- data.frame("time" = Time,
                                         "value" = Value)
  }
))

