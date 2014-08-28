\name{h5CreateFile}
\alias{h5CreateFile}
\title{Function to create file a h5 file}
\usage{
  h5CreateFile(filePath, overwrite)
}
\arguments{
  \item{filePath}{a character denoting the path to the
  location where the h5 file will be written}

  \item{overwrite}{integer 1 for overwrite and 0 for not
  overwrite. Will fail if overwrite is 0}
}
\value{
  int 0
}
\description{
  Function to create a h5 file. It is intended for internal
  use only
}

