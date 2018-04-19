\name{stripApr}
\alias{stripApr}
\alias{stripRet}
\title{Remove Portions of Force Curves}
\description{
  Remove either the approach or retraction portion of a force curve.
}
\usage{
stripApr(frame,zPos)
stripRet(frame,zPos)
}
\arguments{
  \item{frame}{The data.frame containing the force curve to be trimmed,
    most likely of the form produced by \code{loadIBW}}
  \item{zPos}{The name of the column in \code{frame} containing the z
    position data}
}
% add \details
\value{
  A data.frame with the same columns as \code{frame} with either the
  approach or retraction portion removed.
}