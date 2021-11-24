
mvhr <- function(x, WinLen){
  # Inputs x is a raw data consists of spot price in the first column and futures prices from the second column.
  # Make the dataset balanced & remove NaN observations
  # drop the observation with NaN values
  x <- na.omit(x)
  # Check the compatibility of WinLen
  if (WinLen <= 0){
    stop("Window Length should be positive")
  }

  # variable setting
  p = ncol(x)
  n = nrow(x)
  # spot price
  sp = x[,1]
  # futures price
  fp = x[,2:p]
  # take the log difference to remove non-stationarity
  lsp = log(sp)
  ld_sp = lsp[(1+1):n,] - lsp[1:(n-1),]
  lfp = log(fp)
  ld_fp = lfp[(1+1):n,] - lfp[1:(n-1),]


# Minimum variance Hedge ratio
# WinLen = 15       # window length
  OoSLen = WinLen       # out of sample
  nObs = nrow(ld_sp)    # Number of windows
  nFut = ncol(ld_fp)      # Number of futures contracts
  nWin = nObs - WinLen - OoSLen + 1   # Total number of windows

# construct an empty storage for HR
  HR = matrix(0, nrow = nWin, ncol= nFut)

  for (iWin in 1:nWin){
  histSpot = ld_sp[iWin:iWin+WinLen-1,]
  histFut = ld_fp[iWin:iWin+WinLen-1,]
  HR[iWin,] =  sd(histSpot)*cor(histSpot,histFut)/sd(histFut)
  }


# Performance of the hedge ratio
# profit (hedgers hold the contract for 1 day and resell it.)
HE = matrix(0, nrow = nWin, ncol= nFut)

for (iWin in 1:nWin){
OoSSpot = ld_btcs[iWin+WinLen:iWin+WinLen+OoSLen-1,]
OoSFut = ld_fp[iWin+WinLen:iWin+WinLen+OoSLen-1,]
hr_mod = HR[iWin,]*fp(iWin+WinLen,)/btcs(iWin+WinLen)
Prof_nh = OoSSpot
Prof_h = repmat(OoSSpot,1,nFut) - repmat(hr_mod,OoSLen,1).*OoSFut
var_h[iWin,] = var(Prof_h)
var_nh[iWin,] = var(Prof_nh)
HE[iWin,] = (var(Prof_h)-var(Prof_nh))/var(Prof_nh)
}
 return(list(HR = HR, HE = HE))
}


# Semivariance hedge ratio

# initialize the matrix
# Semivariance hedge ratio
semivar <- function(v,w){
meanV = w'*v
SV = w'*(max(repmat(meanV,size(v,1),1)-v, 0)).^2
return(SV)
}

# initialize the matrix
HR0 = 1
HR_sv =  zeros(nWin,nFut)
HE_sv = zeros(nWin,nFut)

for (iWin in 1:nWin){
histSpot = ld_btcs(iWin:iWin+WinLen-1,:)
wHist =  ones(WinLen,1)/WinLen   # weight
wOoS  = ones(OoSLen,1)/OoSLen
OoSSpot = ld_btcs(iWin+WinLen:iWin+WinLen+OoSLen-1,:)
OoSFut = ld_fp(iWin+WinLen:iWin+WinLen+OoSLen-1,:)

# semivariance with no hedging
sv_nh = semivar(OoSSpot, wOoS)

for (iFut in 1: nFut){
SVObj = @(h)SemiVar(OoSSpot - h*(fp(iWin+WinLen,iFut)/btcs(iWin+WinLen))*OoSFut(:,iFut),wHist)
HR_sv(iWin,iFut) = fminsearch(SVObj,HR0)
}

Prof_h_sv = repmat(OoSSpot,1,nFut) - repmat(HR_sv(iWin,:).*(fp(iWin+WinLen,:)/btcs(iWin+WinLen)),OoSLen,1).*OoSFut
sv_h = SemiVar(Prof_h_sv, wOoS)
HE_sv(iWin,:) = (sv_h-sv_nh)/(sv_nh)
return()
}

