# Hedge

Siun Lee


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

