\name{h5DF}
\alias{h5DF}
\title{Specification of h5DF reference class}
\usage{
  h5DF(...)
}
\description{
  The h5DF object is an interface to a special type of HDF5
  file that holds the data for large data frames in chunks.
  Data frames are converted to numeric matrices, characters
  are converted to factors and factors to numeric data. It
  is designed to allow fast access to chunks of data from
  the HDF5 file.
}
\section{Methods in the h5DF object}{
  \itemize{ \item h5DF$new() creates a new h5DF object.
  Users should use the functions ?newH5DF() and
  ?openH5DF().  \item h5DF$createH5DF(DF, filePath,
  chunkSize, ...) populates a h5DF object with data from a
  data frame or csv file containing a data frame DF. It
  writes the meta data to the h5DF object and the dataset
  to a h5 file. Users should use the functions ?newH5DF()
  and ?openH5DF(). \itemize{ \item DF a data frame or path
  to a csv file containing a data frame \item filePath path
  to a file where the h5 object will be written \item
  chunkSize the number of rows that will be written per
  chunk \item ... arguments that will be passed to the
  read.csv() function } \item h5DF$append(DF, ...) appends
  a data frame to the current h5 file and updates the meta
  data in the file and on the object \itemize{ \item DF a
  data frame or path to a csv file containing a data frame
  \item h5DF$readChunk(chunkName) reads chunkName from the
  h5DF object returning a data frame chunk.  \item
  chunkName character name of the chunk to be returned }
  \item h5DF$readChunks(chunks) reads the chunkNames from
  the h5DF object returning a data frame containing all the
  chunks that have been read. \itemize{ \item chunks
  character vector name of the chunks to be binded together
  and returned as a data frame } \item h5DF$readTable()
  reads the whole table back into R. This is a convenience
  function and the user must know whether their system has
  sufficient memory to accomodate the data frame. \item
  h5DF$memorize() this function converts the h5DF object to
  a h5MEMDF object. It reads each chunk to memory and
  supplies pointers in R to access each chunk. Very useful
  when you have lots of memory in your system and need an
  efficient way to access chunks of data. }
}

\section{Fields in the h5DF object}{
  These are the fields in the h5DF object, they are not to
  be directly modified by the user but can be accessed by
  h5DF$fieldName.

  \itemize{ \item nChunks: The number of chunks in the h5DF
  object \item chunkNames: The names of the chunks in the
  h5DF object \item colNames: The column names in the
  submitted data frame \item colClasses: The classes of the
  submitted data frame \item nrows: The number of rows in
  the data frame \item ncols: The number of columns in the
  data frame \item filePath: A character denoting the path
  to the h5 file \item nfactors: The number of factor
  columns in the h5DF object \item factors: A list
  containing the factor levels for each factor in the data
  frame \item chunkSize: The number of rows each chunk will
  contain \item MAT: For internal use when manipulating the
  data frame \item oldNChunks: For internal use. }
}

