\name{saveFits}
\alias{saveFits}
\title{Save Plots of Extracted Fits}
\description{
  Save a .pdf file showing extracted fits plotted on top of the fitted data.
}
\usage{
saveFits(filename,fitData, x="zPos", y="F")
}
\arguments{
  \item{filename}{Where to save the plots}
  \item{fitData}{Fit data produced using the functions
    \code{parExtractStiffness} or \code{parExtractTimeConst}}
  \item{x}{Variable to plot on x axis. Should probably be "zPos" for hertzian fits and "t" for force relaxation fits}
  \item{y}{Variable to plot on y axis}
}
% add \details