\name{newH5MAT}
\alias{newH5MAT}
\title{Function to create a new h5MAT object}
\usage{
  newH5MAT(MAT, filePath, chunkSize, ...)
}
\arguments{
  \item{MAT}{matrix or csv file containing a matrix to be
  converted into a h5MAT object}

  \item{filePath}{character path to the file to be written}

  \item{chunkSize}{the number of rows for each chunk}

  \item{...}{arguments sent to \code{read.csv()} function}
}
\value{
  returns a h5MAT object
}
\description{
  Function to create a new h5MAT object
}
\examples{
n <- 1e3
mat <- matrix(runif(n^2), nc = n)
hmat <- newH5MAT(mat, "mat.h5", 1E2) # a h5MAT object
hmat$append(mat, "mat.h5", 1E2) # append the matrix
ch <- hmat$readChunk("ch2") # reading a chunk into the matrix
ch[1:10, 1:10]
# Reading several chunks
hmat$readChunks(paste("ch", 1:5, sep = ""))
# Reading the table
tab <- hmat$readTable()
# Create a h5MEMMAT object
memtab <- hmat$memorize()
}

