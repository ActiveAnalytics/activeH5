\name{h5WriteInt}
\alias{h5WriteInt}
\title{This function writes an iteger meta data to file}
\usage{
  h5WriteInt(intName, integer, filePath, update)
}
\arguments{
  \item{intName}{the name of the meta data item to be
  written}

  \item{integer}{int that will be written to the meta data
  described by intName}

  \item{filePath}{character path to the h5 file where data
  will be written}

  \item{update}{int flag for whether item is new (0) or
  whether it will overwrite a previous item (1)}
}
\value{
  int 0
}
\description{
  This function is inteded for internal use
}

