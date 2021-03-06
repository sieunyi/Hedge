% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hedge.R
\name{svhr}
\alias{svhr}
\title{Semi variance hedge ratio and its effectiveness}
\usage{
svhr(x, WinLen)
}
\arguments{
\item{x}{(n x 2) matrix of input that consists of spot price in the first column and futures prices in the second column.}

\item{WinLen}{Length of window}
}
\value{
(list(HR_sv = HR_sv, HE_sv = HE_sv))
\item{HR_sv}{Length(nWin x 1) optimal hedge ratio matrix}
\item{HE_sv}{Length(nWin x 1) hedging effectiveness matrix}
}
\description{
Semi variance hedge ratio and its effectiveness
}
\examples{
data(fx)
spot_price = fx$...1
futures_price = fx$...2
x = cbind(spot_price, futures_price)
Winlen <- 15
svhr(x, Winlen)
}
