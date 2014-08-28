\name{h5MAT}
\alias{h5MAT}
\title{Specification of the h5MAT reference class}
\usage{
  h5MAT(...)
}
\description{
  The h5MAT object is an interface to a special type of
  HDF5 file that holds data for large matrices in chunks.
  It is designed to allow fast access to chunks of data
  from the HDF5 file.
}
\section{Methods in the h5MAT object}{
  \itemize{ \item h5MAT$new() creates a new h5MAT object,
  users should use ?newH5MAT() or ?openH5MAT(). \item
  h5MAT$createH5MAT(MAT, filePath, chunkSize, ...)
  populates a h5MAT object with data from MAT. Users should
  use ?newH5MAT() or ?openH5MAT(). \itemize{ \item MAT a
  matrix or a path to a csv file containing a matrix to be
  converted to a h5MAT object \item filePath a character
  denoting a path to a location where the h5MAT object wll
  be written \item chunkSize number of rows denoting the
  size of each matrix chunk \item ... parameters to be
  passed to the read.csv() function } \item
  h5MAT$append(MAT, ...) appends matrix MAT to the current
  h5MAT object \itemize{ \item MAT a matrix or a path to a
  csv file containing a matrix to be converted to a h5MAT
  object \item ... parameters to be passed to the
  read.csv() function } \item h5MAT$readChunk(chunkName)
  reads the chunk denotes by chunkName into R as a matrix
  \itemize{ \item chunkName a character denoting the name
  of the chunk to be returned as a matrix } \item
  h5MAT$readChunks(chunks) read the chunks denotes by
  chunks into R as a matrix \itemize{ \item chunks a
  character vector denoting the chunks to be read into R as
  a single matrix } \item h5MAT$readTable() reads the whole
  matrix in the h5MAT into R as a matrix \item
  h5DF$memorize() this function converts the h5MAT object
  to a h5MEMMAT object. It reads each chunk to memory and
  supplies pointers in R to access each chunk. Very useful
  when you have lots of memory in your system and need an
  efficient way to access chunks of data. }
}

\section{Fields in the h5MAT object}{
  These are the fields in the h5MAT object, they are not to
  be directly modified by the user but can be accessed by
  h5MAT$fieldName.

  \itemize{ \item nChunks: The number of chunks in the
  h5MAT object \item chunkNames: The names of the chunks in
  the h5MAT object \item nrows: The number of rows in the
  data frame \item ncols: The number of columns in the data
  frame \item filePath: A character denoting the path to
  the h5 file \item chunkSize: The number of rows each
  chunk will contain \item MAT: For internal use when
  manipulating the data frame \item oldNChunks: For
  internal use. }
}

