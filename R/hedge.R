
#' minimum variance hedge ratio function
#'
#' @param x Inputs x is a raw data consists of spot price in the first column and futures prices from the second column.
#' @param WinLen Length of window
#'
#' @return return(HR)
#' @export
#'
#' @examples
#' x <- matrix(rnorm(100), 50, 2)
#' Winlen <- 15
#' mvhr(x, Winlen)
mvhr <- function(x, WinLen) {

  # Make the dataset balanced & remove NaN observations
  # drop the observation with NaN values
  x <- na.omit(x)
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
  lsp <- log(sp)
  ld_sp <- lsp[(1 + 1):n, ] - lsp[1:(n - 1), ]
  lfp <- log(fp)
  ld_fp <- lfp[(1 + 1):n, ] - lfp[1:(n - 1), ]


  # Minimum variance Hedge ratio
  # WinLen = 15       # window length
  # out of sample
  OoSLen <- WinLen
  # Number of windows
  nObs <- nrow(ld_sp)
  # Number of futures contracts
  nFut <- ncol(ld_fp)
  # Total number of windows
  nWin <- nObs - WinLen - OoSLen + 1

  # construct an empty storage for HR, history data
  HR <- matrix(0, nrow = nWin, ncol = nFut)
  histSpot <- matrix(0, nrow = WinLen, ncol = 1)
  histFut <- matrix(0, nrow = WinLen, ncol = 1)
  HE <- matrix(0, nrow = nWin, ncol = nFut)


  for (iWin in 1:nWin) {
    # historical spot price
    histSpot <- ld_sp[iWin:iWin + WinLen - 1, ]
    histFut <- ld_fp[iWin:iWin + WinLen - 1, ]
    HR[iWin, ] <- (sd(histSpot) * cor(histSpot, histFut)) / sd(histFut)

  # Out-of-sample is testing the hedge ratio obtained above using samples that were not included in historical data.
  # selecting out of sample for spot price
    OoSSpot <- ld_sp[iWin + WinLen:iWin + WinLen + OoSLen - 1, ]
  # selecting out of sample for futures price
    OoSFut <- ld_fp[iWin + WinLen:iWin + WinLen + OoSLen - 1, ]
  # getting a modified hedge ratio
    hr_mod <- HR[iWin, ] * fp[iWin + WinLen, ] / sp[iWin + WinLen, ]
  # profit when h = 0, which is spot price
    Prof_nh <- OoSSpot
  # profit when h is not 0.
    Prof_h <- matrix(OoSSpot, length(OoSSpot), nFut, byrow=FALSE) - matrix(hr_mod, OoSLen, 1) * OoSFut
  # calculate the hedging effectiveness by comparing the variance of the profits for each portfolio.
    var_h[iWin, ] <- var(Prof_h)
    var_nh[iWin, ] <- var(Prof_nh)
    HE[iWin, ] <- (var(Prof_h) - var(Prof_nh)) / var(Prof_nh)
  }
  return(list(HR = HR, HE = HE))
}
#' Semivariance hedge ratio
#'
#' @param v price matrix
#' @param w weight matrix
#'
#' @return semivariance vector SV
#' @export
#'
#' @examples
semivar <- function(v, w) {
  meanV <- tcrossprod(w, v)
  SV <- tcrossprod(w, (max(matrix(meanV, size(v, 1), 1) - v, 0))^2)
  return(SV)
}



#' Semi variance hedge ratio and its effectiveness
#'
#' @param x Inputs x is a raw data consists of spot price in the first column and futures prices from the second column.
#' @param WinLen Length of window
#'
#' @return
#' @export
#'
#' @examples
svhr <- function(x, WinLen){
  # initialize the matrix
  HR0 <- 1
  HR_sv <- matrix(0, nWin, nFut)
  HE_sv <- matrix(0, nWin, nFut)

  for (iWin in 1:nWin) {
    histSpot <- ld_btcs(iWin:iWin + WinLen - 1, )
    wHist <- ones(WinLen, 1) / WinLen # weight
    num <- matrix(1, nrow = OoSLen, ncol = 1)
    wOoS <- ones[OoSLen, 1] / OoSLen
    OoSSpot <- ld_sp[iWin + WinLen:iWin + WinLen + OoSLen - 1, ]
    OoSFut <- ld_fp[iWin + WinLen:iWin + WinLen + OoSLen - 1, ]

    # semivariance with no hedging
    sv_nh <- semivar(OoSSpot, wOoS)

  for (iFut in 1: nFut){
    v = OoSSpot - h*(fp(iWin+WinLen,iFut)/btcs(iWin+WinLen))*OoSFut[,iFut]
    SVObj = SemiVar(v,wHist)
    HR_sv[iWin,iFut] = fminsearch(SVObj,HR0, minimize=TRUE)
    }

    Prof_h_sv <- repmat(OoSSpot, 1, nFut) - repmat(HR_sv(iWin, ) * (fp(iWin + WinLen, ) / btcs(iWin + WinLen)), OoSLen, 1) * OoSFut
    sv_h <- SemiVar(Prof_h_sv, wOoS)
    HE_sv[iWin, ] <- (sv_h - sv_nh) / (sv_nh)
  }
  return(HR_sv = HR_sv, HE_sv = HE_sv)
}
