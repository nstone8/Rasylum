\name{loadPreSorted}
\alias{loadPreSorted}
\title{Batch Load Files}
\description{
  Load .ibw files from 'folder' with names of the form const1variableRegionconst2variableregion...suffix
}
\usage{
loadPreSorted(folder,consts,suffix)
}
\arguments{
  \item{folder}{The folder from which to load files}
  \item{consts}{A vector of the constant portions of the file names}
  \item{suffix}{The constant end of the filename (usually the file
    extension).}
}

\value{An object containing the data from all .ibw files in the
  folder.} %flesh out with full structure