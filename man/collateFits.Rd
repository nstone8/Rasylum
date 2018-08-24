\name{collateFits}
\alias{collateFits}
\title{Convert Fit Data Into a Data Frame}
\description{
  Create a single data frame containing all fit values and identifiers
}
\usage{
collateFits(fitData)
}
\arguments{
  \item{fitData}{A data object created by \code{parExtractStiffness} or \code{parExtractTimeConst}}
}
% add \details
\value{
  A data.frame containing a summary of all of the fits contained in fitData.
}