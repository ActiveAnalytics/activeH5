\name{h5WriteFactor}
\alias{h5WriteFactor}
\title{Function to write the levels of a factor variable to meta data}
\usage{
  h5WriteFactor(charName, charVec, filePath, update)
}
\arguments{
  \item{charName}{character denoting the meta data name of
  the factor to be written}

  \item{charVec}{characer denoting the factor levels to be
  written}

  \item{filePath}{character denoting the location of the h5
  file}

  \item{update}{int flag for whether item is new (0) or
  whether it will overwrite a previous item (1)}
}
\value{
  int 0
}
\description{
  Function is intended for internal use
}

