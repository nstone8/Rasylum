\name{normalizeFrame}
\alias{normalizeFrame}
\title{Normalize Data in Data Frames}
\description{
  Normalize a column in a data.frame with respect to the lowest or
  highest value in another column.
}
\usage{
normalizeFrame(frame,column,identifiers,wrt,value="lowest")
}
\arguments{
  \item{frame}{A data.frame containing the data to be normalized}
  \item{column}{The name of the column to be normalized}
  \item{identifiers}{List of the column names which can uniquely
    identify each case to be normalized}
  \item{wrt}{Which column to use to identify the measurement to
    normalize to}
  \item{value}{Must be "lowest" or "highest". Sets whether the lowest or
  highest value of column \code{wrt} contains the measurement to
  normalize with respect to}
  }
% add \details
\value{
  A data.frame containing every unique combination of \code{identifiers}
  and the normalized column.
}