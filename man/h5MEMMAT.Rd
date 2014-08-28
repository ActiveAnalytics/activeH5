\name{h5MEMMAT}
\alias{h5MEMMAT}
\title{Specification of the h5MEMMAT reference class}
\usage{
  h5MEMMAT(...)
}
\description{
  The h5MEMMAT class allows the user to create a h5MAT
  object in memory rather than on file. The matrix chunks
  are stored in memory and these are accessed by pointers
  in the h5MEMMAT object.
}
\section{Methods in the h5MEMMAT object}{
  \itemize{ \item h5MEMMAT$new() creates a new h5MEMMAT
  object, users should use ?newH5MAT() or ?openH5MAT().
  \item h5MEMMAT$createH5MAT(MAT, filePath, chunkSize, ...)
  populates a h5MEMMAT object with data from MAT. Users
  should use ?newH5MAT() or ?openH5MAT(). \itemize{ \item
  MAT a matrix or a path to a csv file containing a matrix
  to be converted to a h5MEMMAT object \item filePath a
  character denoting a path to a location where the
  h5MEMMAT object wll be written \item chunkSize number of
  rows denoting the size of each matrix chunk \item ...
  parameters to be passed to the read.csv() function }
  \item h5MEMMAT$append(MAT, ...) appends matrix MAT to the
  current h5MEMMAT object \itemize{ \item MAT a matrix or a
  path to a csv file containing a matrix to be converted to
  a h5MEMMAT object \item ... parameters to be passed to
  the read.csv() function } \item
  h5MEMMAT$readChunk(chunkName) reads the chunk denotes by
  chunkName into R as a matrix \itemize{ \item chunkName a
  character denoting the name of the chunk to be returned
  as a matrix } \item h5MEMMAT$readChunks(chunks) read the
  chunks denotes by chunks into R as a matrix \itemize{
  \item chunks a character vector denoting the chunks to be
  read into R as a single matrix } \item
  h5MEMMAT$readTable() reads the whole matrix in the
  h5MEMMAT into R as a matrix \item
  h5MEMMAT$createH5MAT(filePath) this function converts the
  h5MEMMAT object to a h5MAT object. It writes the data in
  the h5MEMMAT object to filePath and returns a h5MAT
  object. }
}

\section{Fields in the h5MEMMAT object}{
  These are the fields in the h5MEMMAT object, they are not
  to be directly modified by the user but can be accessed
  by h5MEMMAT$fieldName.

  \itemize{ \item nChunks: The number of chunks in the
  h5MEMMAT object \item ptrs: A list of pointers to each of
  the chunks in the matrix \item nrows: The number of rows
  in the data frame \item ncols: The number of columns in
  the data frame \item chunkSize: The number of rows each
  chunk will contain \item MAT: For internal use when
  manipulating the data frame \item oldNChunks: For
  internal use. }
}

