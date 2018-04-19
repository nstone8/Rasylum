\name{stiffnessSphereOnPlane}
\alias{stiffnessSphereOnPlane}
\alias{parExtractStiffness}
\title{Extract Sample Stiffness From Force Curves}
\description{
  Perform a fit of force curves to a Hertzian contact model to estimate
  the Young's modulus of a sample.
}
\usage{
stiffnessSphereOnPlane(rBead, extZ, extForce, CPMaxF=.05,
percentToFit=0.2, roughness=.01, Q=.5, approachTrim=0.2, debug=FALSE)
parExtractStiffness(rBead, cases, zPos="zSensr", force="force", CPMaxF=.05, percentToFit=0.2, roughness=0.05,Q=.5, approachTrim=0.2, debug=FALSE, minRise=0, numCores=-1)
}
\arguments{
  \item{rBead}{Radius of the spherical indenter used, in meters}
  \item{extZ}{Column vector containing z position data}
  \item{extForce}{Column containing force data}
  \item{CPMaxF}{The percentage of the maximum force beyond which this
    function will not search for the contact point. This is the default
    contact point if other methods fail.}
  \item{percentToFit}{Percentage of the extension curve that will be
    used to perform the fits}
  \item{roughness}{The percentage of the force range features must be
    larger than to not be considered noise}
  \item{Q}{The minimum ratio of height to width a trough on a residual
    vs. contact point plot has to have to be considered a local minima.}
  \item{approachTrim}{The percentage of the beginning of a curve to be
    trimmed off before performing fitting}
  \item{debug}{If TRUE, each fit will be displayed to the user, who
    should press 'enter' at the command prompt to cycle through the
    fits. This functionality is intended to be used for tuning of fit
    parameters}
  \item{cases}{An object of the type produced by \code{loadPreSorted}
    containing the force curves to be fit to the Hertzian Contact Model}
  \item{zPos}{The name of the columns containing the z position data}
  \item{force}{the name of the columns containing the force data}
  \item{minRise}{The percentage of the total force range that the fit
    curve must traverse to not be considered degenerate. Degenerate
    curves will be refit}
  \item{numCores}{Number of cores to use for the computation. If
    \code{numCores}==-1 all available cores will be used}
}
%add \details
\value{An object containing the extracted stiffness values, fit
  residuals and the fit curves themselves. Fit curves can be saved using
\code{saveFits}, other data can be formatted into a human-readable
data.frame using \code{collateFits}.}