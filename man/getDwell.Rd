\name{getDwell}
\alias{getDwell}
\title{Extract Dwell from Force Curves}
\description{
  Extract only the dwell from a force curve.
}
\usage{
getDwell(frame,dwellTime,zPos,F,t,extraTrim=0)
}
\arguments{
  \item{frame}{The data.frame containing the force curve to be trimmed,
    most likely of the form produced by \code{loadIBW}}
  \item{dwellTime}{Length of dwell. This value is extracted by
    \code{loadIBW} and \code{batchLoad}}.
  \item{zPos}{The name of the column in \code{frame} containing the z
    position data}
  \item{F}{The name of the column in \code{frame} containing the force
    data}
  \item{t}{The name of the column in \code{frame} containing the time
    data}
  \item{extraTrim}{Percentage of dwell time to trim off the end of the
    dwell curve}
}
% add \details
\value{
  A data.frame with the same columns as \code{frame} with only the dwell
  portion remaining.
}