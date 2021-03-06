#' minimum variance hedge ratio function
#' @description This function will calculate the single commodity hedge ratio by minimizing the variance of the returns.
#' @param x (n x 2) matrix of input that consists of spot price in the first column and futures prices in the second column.
#' @param WinLen Length of window, any positive integer
#'
#' @return (list(HR = HR, HE = HE))
#' \item{HR}{Length(nWin x 1) optimal hedge ratio vector over the sample period}
#' \item{HE}{Length(nWin x 1) hedging effectiveness vector over the sample period}
#' @export
#'
#' @examples
#' data(fx)
#' spot_price = fx$...1
#' futures_price = fx$...2
#' x = cbind(spot_price, futures_price)
#' Winlen <- 15
#' mvhr(x, Winlen)
mvhr <- function(x, WinLen) {

  # Make the dataset balanced & remove NaN observations
  # drop the observation with NaN values
  x <- stats::na.omit(x)
  # Check the compatibility of WinLen
  if (WinLen <= 0) {
    stop("Window Length should be positive")
  }

  # variable setting
  p <- ncol(x)
  n <- nrow(x)
  # spot price
  sp <- x[, 1]
  # futures price
  fp <- x[, 2:p]
  # take the log difference to remove non-stationarity
  ld_sp <- diff(log(sp))
  ld_fp <- diff(log(fp))

  ld_sp <- as.matrix(ld_sp)
  sp <- as.matrix(sp)

  # Minimum variance Hedge ratio
  # WinLen = 15       # window length
  # out of sample
  OoSLen <- WinLen
  # Number of windows
  nObs <- nrow(ld_sp)
  # Number of futures contracts
  nFut <- p-1
  # Total number of windows
  nWin <- nObs - WinLen - OoSLen + 1

  if (nFut==1){
    ld_fp = as.matrix(ld_fp)
    fp = as.matrix(fp)
  }

  # construct an empty storage for HR, history data
  HR <- matrix(0, nrow = nWin, ncol = nFut)
  histSpot <- matrix(0, nrow = WinLen, ncol = 1)
  histFut <- matrix(0, nrow = WinLen, ncol = 1)
  HE <- matrix(0, nrow = nWin, ncol = nFut)
  var_h <- matrix(0, nrow = nWin, ncol = nFut)
  var_nh <- matrix(0, nrow = nWin, ncol = nFut)


  for (iWin in 1:nWin) {
    # historical spot price

    histSpot <- ld_sp[iWin:(iWin + WinLen - 1), ]
    histFut <- ld_fp[iWin:(iWin + WinLen - 1), ]
    HR[iWin, ] <- (stats::sd(histSpot) * stats::cor(histSpot, histFut)) / stats::sd(histFut)

    # Out-of-sample is testing the hedge ratio obtained above using samples that were not included in historical data.
    # selecting out of sample for spot price
    OoSSpot <- ld_sp[(iWin + WinLen):(iWin + WinLen + OoSLen - 1), ]
    # selecting out of sample for futures price
    OoSFut <- ld_fp[(iWin + WinLen):(iWin + WinLen + OoSLen - 1), ]
    # getting a modified hedge ratio
    # hr_mod is a scalar

    hr_mod <- HR[iWin, ] * fp[(iWin + WinLen), ] / sp[(iWin + WinLen), ]
    # prof_nh is length(WinLen) column vector when h = 0, which is equal to spot price.
    Prof_nh <- OoSSpot
    # prof_h is length(WinLen) x nFut matrix when h is not 0.
    Prof_h <- matrix(OoSSpot, length(OoSSpot), nFut, byrow = FALSE) - as.numeric(matrix(hr_mod, OoSLen, 1)) * as.matrix(OoSFut)
    # calculate the hedging effectiveness by comparing the variance of the profits for each portfolio.
    var_h[iWin, ] <- stats::var(Prof_h)
    var_nh[iWin, ] <- stats::var(Prof_nh)
    HE[iWin, ] <- (stats::var(Prof_h) - stats::var(Prof_nh)) / stats::var(Prof_nh)
  }
  return(list(HR = HR, HE = HE))
}


#' EURO/USD exchange rate and futures price of EURO.
#' A dataset containing the prices and other attributes of almost 20 years
#'
#' @format A data frame with 5047 rows and 2 variables:
#' \describe{
#'   \item{...1}{EURO/USD exchange rate}
#'   \item{...2}{EURO futures price data}
#' }
#' @source \url{http://www.bloomberg.com/}
#'
"fx"




#' Semivariance hedge ratio
#' @description This function will calculate the single commodity hedge ratio by minimizing the semi variance of the returns.
#' @param v length(WinLen x 1) price vector
#' @param w length(WinLen x 1) column vector for weight
#'
#' @return semivariance of the input vector, returns a scalar
#' @export
#' @examples
#' WinLen=15
#' w <- (matrix(1, WinLen, 1)) / WinLen
#' v <- matrix(runif(WinLen, min=0, max=10), WinLen, 1)
#' semivar(v,w) #return scalar
semivar <- function(v, w) {
  meanV <- as.vector(crossprod(w, v))
  SV <- crossprod(w, (pmax(meanV - v, 0))^2)
  return(SV) # return scalar
}


#' Semi variance hedge ratio and its effectiveness
#' @param x (n x 2) matrix of input that consists of spot price in the first column and futures prices in the second column.
#' @param WinLen Length of window, any positive integer
#'
#' @return (list(HR_sv = HR_sv, HE_sv = HE_sv))
#' \item{HR_sv}{Length(nWin x 1) optimal hedge ratio vector over the sample period}
#' \item{HE_sv}{Length(nWin x 1) hedging effectiveness vector over the sample period}
#' @export
#'
#'
#' @examples
#' data(fx)
#' spot_price = fx$...1
#' futures_price = fx$...2
#' x = cbind(spot_price, futures_price)
#' Winlen <- 15
#' svhr(x, Winlen)
svhr <- function(x, WinLen) {

  # Make the dataset balanced & remove NaN observations
  # drop the observation with NaN values
  x <- stats::na.omit(x)
  # Check the compatibility of WinLen
  if (WinLen <= 0) {
    stop("Window Length should be positive")
  }

  # variable setting
  p <- ncol(x)
  n <- nrow(x)
  # spot price
  sp <- x[, 1]
  # futures price
  fp <- x[, 2:p]
  # take the log difference to remove non-stationarity
  ld_sp <- diff(log(sp))
  ld_fp <- diff(log(fp))

  ld_sp <- as.matrix(ld_sp)
  sp <- as.matrix(sp)

  # Minimum variance Hedge ratio
  # WinLen = 15       # window length
  # out of sample
  OoSLen <- WinLen
  # Number of windows
  nObs <- nrow(ld_sp)
  # Number of futures contracts
  nFut <- p-1
  # Total number of windows
  nWin <- nObs - WinLen - OoSLen + 1

  if (nFut==1){
    ld_fp = as.matrix(ld_fp)
    fp = as.matrix(fp)
  }

  # initialize the matrix
  #  HR0 <- 1
  HR_sv <- matrix(0, nWin, nFut)
  HE_sv <- matrix(0, nWin, nFut)

  # construct an empty storage for HR, history data
  histSpot <- matrix(0, nrow = WinLen, ncol = 1)
  OoSSpot <- matrix(0, nrow = WinLen, ncol = 1)
  OoSFut <- matrix(0, nrow = WinLen, ncol = 1)
  wHist <- (matrix(1, WinLen, 1)) / WinLen
  num <- matrix(1, nrow = OoSLen, ncol = 1)
  wOoS <- num / OoSLen

  for (iWin in 1:nWin) {
    histSpot <- as.matrix(ld_sp[iWin:(iWin + WinLen - 1), ])
    # weight for the historical price
    OoSSpot <- as.matrix(ld_sp[(iWin + WinLen):(iWin + WinLen + OoSLen - 1), ])
    OoSFut <- as.matrix(ld_fp[(iWin + WinLen):(iWin + WinLen + OoSLen - 1), ])
    for (iFut in 1:nFut) {
      SVObj <- function(h) semivar(matrix(as.numeric(OoSSpot - h * (fp[(iWin + WinLen), iFut] / sp[(iWin + WinLen), ]) * as.matrix(OoSFut, nrow = WinLen)[, iFut]), nrow=WinLen, 1), wHist)
      result <- stats::optimize(SVObj, interval = c(-10, 10), maximum = FALSE)
      HR_sv[iWin, iFut] <- result$minimum
    }
    # semivariance with no hedging
    sv_nh <- semivar(OoSSpot, wOoS)
    Prof_h_sv <- matrix(OoSSpot, length(OoSSpot), nFut) - as.numeric(matrix(HR_sv[iWin, ] * (fp[(iWin + WinLen), ] / sp[(iWin + WinLen), ]), length(OoSSpot), nFut)) * as.matrix(OoSFut)
    sv_h <- semivar(Prof_h_sv, wOoS)
    HE_sv[iWin, ] <- (sv_h - sv_nh) / (sv_nh)
  }

  return(list(HR_sv = HR_sv, HE_sv = HE_sv))

}
