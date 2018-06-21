\name{extractStiffness}
\alias{extractStiffness}
\alias{parExtractStiffness}
\title{Extract Sample Stiffness From Force Curves}
\description{
  Perform a fit of force curves to a Hertzian contact model to estimate
  the Young's modulus of a sample.
}
\usage{
extractStiffness(case,r,approachLength=.1,contactLength=.1,
  searchWidth=.2,maxF=.5,weight=4,correctVirtDefl=TRUE,zPos="zSensr",
  force="force")
parExtractStiffness(cases,r,approachLength=.1,contactLength=.1,
  searchWidth=.2,maxF=.5,weight=4,correctVirtDefl=TRUE,zPos="zSensr",
  force="force",numCores=-1)
}
\arguments{
  \item{case}{One entry of the list produced by \code{loadPreSorted} or
    \code{readARDF} containing the force curve to be fit to the Hertzian
    Contact Model}
  \item{r}{Radius of the spherical indenter used, in meters}
  \item{approachLength}{Percent of the approach curve that can be safely
    assumed not to be in contact with the sample}
  \item{contactLength}{Percent of the approach curve that can be safely
    assumed to be in contact with the sample}
  \item{searchWidth}{Percent of the curve around a rough guess to search
    for the contact point. A warning will be triggered if the contact
    point is not detected in the middle 50 percent of this range}
  \item{maxF}{Upper bound on fit region, in terms of percentage of the
    maximum force}
  \item{weight}{Relative weight to apply to the residual of the linear
    portion of the fit as compared to the nonlinear portion. Increasing
    this value will tend to bring the contact point closer to the approach
    line.}
  \item{correctVirtDefl}{If \code{TRUE}, attempt to correct for non-flat
    non-contact portions of the approach curve}
  \item{zPos}{The name of the columns in \code{case$data} containing the
    z position data}
  \item{force}{the name of the columns in \code{case$data} containing
    the force data}
  \item{cases}{An object of the type produced by \code{loadPreSorted}
    containing the force curves to be fit to the Hertzian Contact Model}
  \item{numCores}{Number of cores to use for the computation. If
    \code{numCores}==-1 all available cores will be used}
}
%add \details
\value{An object containing the extracted stiffness values, fit
  residuals and the fit curves themselves. The fit curves which will have
  been corrected in order to place the detected contact point at (0,0).
  Fit curves can  be saved using \code{saveFits}, other data can be
  formatted into a  human-readable data.frame using \code{collateFits}.}