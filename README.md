# Hedge

Siun Lee

## Intended Use of This Package
"Hedge” is a package to analyze whether futures are an effective tool for hedging and how effective they are based on the calculated optimal hedge ratio. Investors want to minimize their risk of returns and hedging with futures price is one common way to offset the price volatility due to their high correlation. The optimal hedge ratio is calculated through minimizing the semi-variance of investors’ return, a method to derive the downside risk reducing. While the variance measures the volatility of the asset returns, semi-variance only considers the negative fluctuations of the returns neutralizing all values above the mean, or above an investor’s target return. After obtaining the optimal hedge ratio, hedging effectiveness is going to be calculated by comparing the two strategies of ”Hedging” and ”No hedging” for the portfolio. For the initial version of this package only considers "single commodity" hedging.


## Installation

To install this package, copy and paste the code below into your console:
```r
library(devtools)
devtools::install_github("sieunyi/Hedge", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```

## Usage

To use this package, copy and paste the code below into your console:
```r
library(Hedge)
```

### Prediction Plot for Spline Terms

To properly utilize this package, you first need to import data that consist of spot and futures price.
```r
HEIdata = read.csv(paste(getwd(),"/data/fxdata.csv", sep = ''), header = TRUE)
```

### WHAT'S LEFT FOR THE REMAINDER OF THE PROJECT
I still need to work on my functions to be run without errors in R. I want to make test.R part more rigorously with visual example. If time's allowed, I would like to work on vignette.  
