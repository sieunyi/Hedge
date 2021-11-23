# Inputs are raw data with daily frequency
# Make the dataset balanced & remove NaN observations

x = matrix(rnorm(80), 40, 4)

# a function that calculates minimum variance hedge ratio
mvhr <- function(x, WinLen,  ){

  # drop the observation with NaN values
  x <- complete.cases(x)
  if (WinLen <= 0){
    stop("Window Length should be positive")
  }

  # variable setting
  p = ncol(x)
  sp = x[,1]        # spot price
  fp = x[,2:p]             # futures price
  ld_sp = diff(log(sp))
  ld_fp = diff(log(fp))

  # ADF test: Null: Unit root is present. The series is randomwalk.
  adf.test(ld_fp)


  # Minimum variance Hedge ratio
#  WinLen = 15       # window length
  OoSLen = WinLen       # out of sample
  nObs = size(ld_sp, 1)    # Number of windows
  nFut = size(ld_fp, 2)      # Number of futures contracts
  nWin = nObs - WinLen - OoSLen + 1   # Total number of windows
  HR = zeros(nWin, nFut)       # Array for hedge ratios

  for (iWin in 1:nWin){
  histSpot = ld_btcs(iWin:iWin+WinLen-1,:)
  histFut = ld_fp(iWin:iWin+WinLen-1,:)
  HR(iWin,:) = OptHR_MV(histSpot,histFut)
  }

# Performance of the hedge ratio
# profit (hedgers hold the contract for 1 day and resell it.)
HE = zeros(nWin,nFut)
for (iWin in 1:nWin){
OoSSpot = ld_btcs(iWin+WinLen:iWin+WinLen+OoSLen-1,:)
OoSFut = ld_fp(iWin+WinLen:iWin+WinLen+OoSLen-1,:)
hr_mod = HR(iWin,:).*fp(iWin+WinLen,:)/btcs(iWin+WinLen)
Prof_nh = OoSSpot
Prof_h = repmat(OoSSpot,1,nFut) - repmat(hr_mod,OoSLen,1).*OoSFut
var_h(iWin,:) = var(Prof_h)
var_nh(iWin,:) = var(Prof_nh)
HE(iWin,:) = (var(Prof_h)-var(Prof_nh))/var(Prof_nh)
}


# Semivariance hedge ratio

# initialize the matrix
# Semivariance hedge ratio
semivar <- function(v,w){
meanV = w'*v
SV = w'*(max(repmat(meanV,size(v,1),1)-v, 0)).^2
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

