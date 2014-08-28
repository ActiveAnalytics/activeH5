\name{h5ModelFrame}
\alias{h5ModelFrame}
\title{Fast model frame for activeReg}
\usage{
  h5ModelFrame(chunkName, selCols_, filePath)
}
\arguments{
  \item{chunkName}{character name of the chunk to be read}

  \item{selCols}{character vector of columns to select}

  \item{filePath}{character path to file where chunk is to
  be read from}
}
\value{
  list representing a data frame with no NA values.
}
\description{
  Function returns a scaled down model frame essentially
  returning list with no NA values. Each item in the list
  represents a column in the data frame.
}

