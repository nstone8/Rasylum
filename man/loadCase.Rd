\name{loadCase}
\alias{loadCase}
\title{Load IBW as a Case Object}
\description{
  Load an IBW File as a \code{case}, one entry of the list produced by
  \code{loadPreSorted}
}
\usage{
loadCase(filename,ident=NA)
}
\arguments{
  \item{filename}{Path to the IBW file to load}
  \item{ident}{A one row \code{data.frame}. Ident names should be given
  by the column names, values by the corresponding entries}.
}
% add \details
\value{
  A \code{case} object, one entry of the list produced by \code{loadPreSorted}
}