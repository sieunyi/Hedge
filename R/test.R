source("hedge.R")
library(readr)
fxdata <- read_csv("data/fxdata.csv")
x <-fxdata

# obtain the hr and he when WinLen =15
result <- mvhr(x, WinLen=15)
plot(result$HE)

  ### to do

  data format 바꾸기
  r-cmd --> push ---> badge
  testthat::
  readme file 바꾸기
  citation??
