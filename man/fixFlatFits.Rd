\name{fixFlatFits}
\alias{fixFlatFits}
\alias{forceNonFlatFits}
\title{Fix Degenerate Stiffness Fits}
\description{
Sometimes \code{parExtractStiffness} fits portions of the force curve
before the probe is in contact (resulting in a stiffness of ~0). These
functions attempts to find these degenerate cases and produce a better
fit.
}
\usage{
fixFlatFits(fits, minRise=.1,debug=FALSE, numCores=-1)
forceNonFlatFits=function(fits,minRise=0.3,numCores=-1)
}
\arguments{
  \item{fits}{Fit data produced using the function
    \code{parExtractStiffness}}
  \item{minRise}{The percentage of the total force range that the fit
    curve must traverse to not be considered degenerate.}
  \item{debug}{if \code{debug}==TRUE each fit will be plotted (the user
    should press 'enter' to cycle through fits) in order to allow for
    tuning of fit parameters.}
  \item{numCores}{Number of cores to use for the computation. If
    \code{numCores}==-1 all available cores will be used}
}
\details{
If the fits being operated on by these functions cannot be fixed while
honoring the other fit parameters set via \code{parExtractStiffness}
\code{fixFlatFits} will leave the fit unchanged wheras
\code{forceNonFlatFits} will increase the \code{CPMaxF} parameter until
the \code{minRise} parameter can be satisfied.
}
\value{A new object of the type produced by \code{parExtractStiffness}
  containing the updated fits}