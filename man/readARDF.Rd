\name{read.ardf}
\alias{read.ardf}
\title{Read .ardf files}
\description{
Import data from force maps saved in Asylum's proprietary .ardf format.
}
\usage{
read.ardf(filename)
}
\arguments{
  \item{filename}{file containing the force map to be read}
}
%add \details
\value{A list object with 2 named components. \code{headers} contains a
  named list of all of the variables defined in the file
  headers. \code{data} contains a named list of data frames. The names
  correspond to the name of each force curve in the original file and
  the column names of each data frame correspond to the name of each
  variable recorded as part of that force curve.}