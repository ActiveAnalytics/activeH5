\name{openH5MAT}
\alias{openH5MAT}
\title{Function to create a h5MAT object from a saved h5 file}
\usage{
  openH5MAT(filePath)
}
\arguments{
  \item{filePath}{path to file containing h5MAT object}
}
\value{
  returns a h5MAT object
}
\description{
  Function to create a h5MAT object from a saved h5 file
}
\examples{
n <- 1e3
mat <- matrix(runif(n^2), nc = n)
hmat <- newH5MAT(mat, "mat.h5", 1E2) # a h5MAT object
hmat2 <- openH5MAT("mat.h5")
}

