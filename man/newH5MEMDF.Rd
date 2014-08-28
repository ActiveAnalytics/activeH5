\name{newH5MEMDF}
\alias{newH5MEMDF}
\title{Function to create a new h5MEMDF object}
\usage{
  newH5MEMDF(df, chunkSize, ...)
}
\arguments{
  \item{df}{data frame or file path to csv file containing
  data frame to be written to h5DF object}

  \item{chunkSize}{the number of rows that each chunk will
  take.}

  \item{...}{parameters passed to read.csv() function}
}
\value{
  a h5MEMDF object
}
\description{
  Function to create a new h5MEMDF object
}
\examples{
# Create a new h5MEMDF object from the iris dataset
ir1 <- newH5MEMDF(iris[1:100,], 10)
ir1 # The details of the object
ir1$readChunk(1) # read chunk number 3
ir1$append(iris[101:150,]) # appending data
}

