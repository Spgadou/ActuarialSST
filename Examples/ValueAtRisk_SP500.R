#### Working VaR example ####

dirname <- "/Examples"
filename <- "/ValueAtRisk_SP500.R"

# install.packages("pdfetch")
# install.packages("zoo")

## Load VaR class
targetScript <- "/Risk measures/VaR.R"

pathstring <- rstudioapi::getSourceEditorContext()$path
newPathstring <- substr(pathstring, 1, nchar(pathstring) - nchar(filename))
newPathstring <- substr(newPathstring, 1, nchar(newPathstring) - nchar(dirname))
newPathstring <- paste(newPathstring, targetScript, sep = "")

source(newPathstring, chdir = TRUE)

## Load stock data
initialData <- pdfetch::pdfetch_YAHOO("^GSPC",fields="close",from="1993-01-01",to="2018-09-28")
P. <- zoo::coredata(initialData) ## Closing prices
R. <- -diff(P.) ## Time index
t. <- zoo::index(initialData)[-1] ## Relative returns (losses)

## Create object with underlying data
x <- VaR$new(R., t.)
x

## Add types of VaR you want
x$kernel(alpha = 0.99, h = 0.01, lookback = 250, Kernel = function(x){
  (3/4) * (1 - x^2) * (x >= -1) * (x <= 1)
})
x$gaussian(alpha = 0.99, lam = 0.99, lookback = 250)

## Visualize the data + VaRs
x$Plot()

