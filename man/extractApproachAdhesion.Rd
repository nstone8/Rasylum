\name{extractApproachAdhesion}
\alias{extractApproachAdhesion}
\alias{extractRetractionAdhesion}
\alias{parExtractApproachAdhesion}
\alias{parExtractRetractionAdhesion}
\title{Extract Adhesion Force From Force Curves}
\description{
  Extract adhesion force during AFM probe approach or retraction.
}
\usage{
extractApproachAdhesion(case,percentFlat=0.8)
extractRetractionAdhesion(case,percentFlat=0.8)
parExtractApproachAdhesion(cases,percentFlat=0.8,numCores=-1)
parExtractRetractionAdhesion(cases,percentFlat=0.8,numCores=-1)
}
\arguments{
  \item{case}{A force curve as produced by loadIBW}
  \item{percentFlat}{The percentage of the approach or retraction curve
    before contact that can be considered flat}
  \item{numCores}{Number of cores to use for the computation. If
    \code{numCores}==-1 all available cores will be used}
}
%add \details
\value{}