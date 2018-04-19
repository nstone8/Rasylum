\name{extractTimeConst}
\alias{extractTimeConst}
\alias{parExtractTimeConst}
\title{Extract Viscous Relaxation Time Constants From Force Curves}
\description{
  Perform a fit of viscous relaxation data to a biexponential decay
  model appropriate for the relaxation of cells indented using an AFM.
}
\usage{
extractTimeConst(frame, time="t", force="force",
zPos="zSensr",dwellTime="dwell", debug=FALSE)
parExtractTimeConst(cases, time="t", force="force", zPos="zSensr", dwell="dwell", debug=FALSE, numCores=-1)
}
\arguments{
  \item{frame}{A data.frame containing the force curve of interest}
  \item{time}{The name of the column in \code{frame} which contains the
    time data}
  \item{force}{The name of the column in \code{frame} which contains the
    force data}
  \item{zPos}{The name of the column in \code{frame} which contains the
    z position data}
  \item{dwellTime}{The name of the column in \code{frame} which contains
    the dwell time}
  \item{debug}{If TRUE, each fit will be displayed to the user, who
    should press 'enter' at the command prompt to cycle through the
    fits. This functionality is intended to be used for tuning of fit
    parameters}
  \item{numCores}{Number of cores to use for the computation. If
    \code{numCores}==-1 all available cores will be used}
}
%add \details
\value{An object containing the extracted stiffness values, fit
  residuals and the fit curves themselves. For fits produced using
  \code{parExtractTimeConst}, fit curves can be saved using
  \code{saveFits} and other data can be formatted into a human-readable
  data.frame using \code{collateFits}.}