\name{h5WriteDoubleMat}
\alias{h5WriteDoubleMat}
\title{Function to write a matrix chunk to file}
\usage{
  h5WriteDoubleMat(dset, chunk, dim, filePath)
}
\arguments{
  \item{dset}{character denoting the meta data name of the
  data set}

  \item{chunk}{matrix that will be written to h5file}

  \item{dim}{numeric containing the dimension of the matrix
  that will be written to file}

  \item{filePath}{character denoting the location of the h5
  file}
}
\value{
  int 0
}
\description{
  Function is intended for internal use
}

