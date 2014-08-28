\name{h5WriteCharVector}
\alias{h5WriteCharVector}
\title{This function writes a character vector to the meta data}
\usage{
  h5WriteCharVector(charName, charVec, filePath, update)
}
\arguments{
  \item{charName}{the name that will be given to the meta
  data character vector}

  \item{charVec}{the character vector to be written as meta
  data}

  \item{filePath}{the path to the h5 file where the data
  will be written}

  \item{update}{integer denoting whether the data item is
  new or whether it is an update (which will overwrite any
  previous item)}
}
\value{
  int 0
}
\description{
  This function writes a character vector to the meta data
  and is intended for internal use.
}

