\name{h5ChunkList}
\alias{h5ChunkList}
\title{Function returns a data frame chunk as a list object to R where the final cbind will take place}
\usage{
  h5ChunkList(chunkName, filePath)
}
\arguments{
  \item{chunkName}{the name of the chunk to be read}

  \item{filePath}{the path to the h5 file}
}
\value{
  List representing chunk data frame
}
\description{
  Function returns a data frame chunk as a list object to R
  where the final cbind will take place
}

