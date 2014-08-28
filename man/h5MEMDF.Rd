\name{h5MEMDF}
\alias{h5MEMDF}
\title{Specification of the h5MEMDF reference class}
\usage{
  h5MEMDF(...)
}
\description{
  The h5MEMDF class allows the user to create a h5DF object
  in memory rather than on file. The data frame chunks are
  stored in memory and these are accessed by pointers in
  the h5MEMDF object.
}
\section{Methods in the h5MEMDF object}{
  \itemize{ \item h5MEMDF$new() creates a new h5MEMDF
  object. Users should use the functions ?newH5MEMDF().
  \item h5MEMDF$createH5DF(df, chunkSize, ...) populates a
  h5MEMDF object with data from a data frame or csv file
  containing a data frame df. It writes the meta data to
  the h5MEMDF object and the dataset to a h5 file. Users
  should use the functions ?newH5MEMDF(). \itemize{ \item
  df a data frame or path to a csv file containing a data
  frame \item chunkSize the number of rows that will be
  written per chunk \item ... arguments that will be passed
  to the read.csv() function } \item h5MEMDF$append(df,
  ...) appends a data frame to the current h5 file and
  updates the meta data in the file and on the object
  \itemize{ \item df a data frame or path to a csv file
  containing a data frame \item ... arguments that will be
  passed to the read.csv() function } \item
  h5MEMDF$readChunk(chunkNum) reads chunkNum from the
  h5MEMDF object returning a data frame chunk. \itemize{
  \item chunkNum numeric number of the chunk to be returned
  } \item h5MEMDF$createH5DF(filePath) creates a h5DF
  object from the h5MEMDF object writing the data to file
  provided by filePath. \itemize{ \item filePath character
  denoting location for where the h5 file should be
  written. } }
}

\section{Fields in the h5MEMDF object}{
  These are the fields in the h5MEMDF object, they are not
  to be directly modified by the user but can be accessed
  by h5MEMDF$fieldName.

  \itemize{ \item nChunks: The number of chunks in the
  h5MEMDF object \item ptrs: A list of pointers to each of
  the chunks in the data frame \item colNames: The column
  names in the submitted data frame \item colClasses: The
  classes of the submitted data frame \item nrows: The
  number of rows in the data frame \item ncols: The number
  of columns in the data frame \item nfactors: The number
  of factor columns in the h5MEMDF object \item factors: A
  list containing the factor levels for each factor in the
  data frame \item chunkSize: The number of rows each chunk
  will contain \item DF: For internal use when manipulating
  the data frame \item oldNChunks: For internal use. }
}

