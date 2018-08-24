\name{loadIBW}
\alias{loadIBW}
\title{Load Igor Binary Wave Files}
\description{
  Load AFM force curves from .ibw files and extract calibration values.
}
\usage{
loadIBW(wave,asDataFrame=FALSE)
}
\arguments{
  \item{wave}{The file to load}
  \item{asDataFrame}{If TRUE, return all data as a data.frame, rather
than as a list}
}
% add \details
\value{
  If asDataFrame==FALSE this function returns a named list with the
  following components. Otherwise it returns a dataframe with the
  following columns.
\item{t}{Vector containing the time at each datapoint}
\item{zSensr}{Vector containing the Z position at each datapoint}
\item{rawZSensr}{Vector containing the raw Z position at each datapoint}
\item{defl}{Vector containing the deflection of cantilever at each datapoint}
\item{force}{Vector containing the force exerted by the cantilever at
each datapoint}
\item{filename}{The name of the file the data was loaded from}
\item{inVols}{The inverse optical lever sensitivity recorded in the .ibw
file}
\item{k}{The cantilever stiffness indicated in the .ibw file}
\item{dwell}{How long of a dwell was performed while capturing this
force curve}
}