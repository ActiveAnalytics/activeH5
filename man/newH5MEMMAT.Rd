\name{newH5MEMMAT}
\alias{newH5MEMMAT}
\title{Function for instantiating a h5MEMMAT object}
\usage{
  newH5MEMMAT(MAT, chunkSize, ...)
}
\arguments{
  \item{MAT}{the matrix or path to a csv file containing a
  matrix to be written to a h5MEMMAT object}

  \item{chunkSize}{the number of rows in each chunk}

  \item{...}{arguments passed to read.csv() file}
}
\value{
  a h5MEMMAT object
}
\description{
  Function for instantiating a h5MEMMAT object
}
\examples{
cat(getwd(), "\\n")
n <- 1e3
mat <- matrix(runif(n^2), nc = n)
hmat <- newH5MEMMAT(mat, 10) # creating the h5MEMMAT object
hmat$append(mat, 10) # appending to the h5MEMMAT object
hmat$readChunk(2)[,1:6] # reading the second chunk
# Writing a h5MAT object to file
hmat2 <- newH5MAT(mat, "mat.h5", 10)
hmat2 # this is a h5MAT object
# Reading the matrix to memory
hmat3 <- hmat2$memorize()
}

