\name{loadARDF}
\alias{loadARDF}
\title{Batch Load Points from Force Map}
\description{
  Load individual force curves from output of read.ardf of .ARDF force map files into data frame usuable by other Rasylum functions
}
\usage{
loadARDF(ardf)
}
\arguments{
  \item{ardf}{The output of read.ardf function}
}

\value{An object containing the data from all individual force curves in an ARDF map - same structure as the output of loadPreSorted} %flesh out with full structure