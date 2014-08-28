# This file contains the class descriptions for all the h5-based objects

#------------------------------------------------------------------------------------------------
# h5DF class specification
#------------------------------------------------------------------------------------------------

#' @title Specification of h5DF reference class
#' 
#' @description The h5DF object is an interface to a special type of HDF5 file that holds the data
#' for large data frames in chunks. Data frames are converted to numeric matrices, characters are
#' converted to factors and factors to numeric data. It is designed to allow fast access to 
#' chunks of data from the HDF5 file.
#' @section Methods in the h5DF object:
#' \itemize{
#'  \item h5DF$new() creates a new h5DF object. Users should use the functions ?newH5DF() and ?openH5DF().
#'  \item h5DF$createH5DF(DF, filePath, chunkSize, ...) populates a h5DF object with data from
#' a data frame or csv file containing a data frame DF. It writes the meta data to the h5DF object
#' and the dataset to a h5 file. Users should use the functions ?newH5DF() and ?openH5DF().
#' \itemize{
#'          \item DF a data frame or path to a csv file containing a data frame
#'          \item filePath path to a file where the h5 object will be written
#'          \item chunkSize the number of rows that will be written per chunk
#'          \item ... arguments that will be passed to the read.csv() function
#' }
#' \item h5DF$append(DF, ...) appends a data frame to the current h5 file and updates the meta data
#' in the file and on the object
#' \itemize{
#'          \item DF a data frame or path to a csv file containing a data frame
#'          \item h5DF$readChunk(chunkName) reads chunkName from the h5DF object returning a data frame chunk.
#'          \item chunkName character name of the chunk to be returned
#' }
#' \item h5DF$readChunks(chunks) reads the chunkNames from the h5DF object returning a
#' data frame containing all the chunks that have been read.
#' \itemize{
#'          \item chunks character vector name of the chunks to be binded together and returned as a data frame
#' }
#' \item h5DF$readTable() reads the whole table back into R. This is a convenience function and the
#' user must know whether their system has sufficient memory to accomodate the data frame.
#' \item h5DF$memorize() this function converts the h5DF object to a h5MEMDF object. It reads each chunk
#' to memory and supplies pointers in R to access each chunk. Very useful when you have lots of memory
#' in your system and need an efficient way to access chunks of data.
#' }
#'@section Fields in the h5DF object:
#'
#' These are the fields in the h5DF object, they are not to be directly modified by the user but can 
#' be accessed by h5DF$fieldName.
#'
#'\itemize{
#'\item nChunks: The number of chunks in the h5DF object
#'\item chunkNames: The names of the chunks in the h5DF object
#'\item colNames: The column names in the submitted data frame
#'\item colClasses: The classes of the submitted data frame
#'\item nrows: The number of rows in the data frame
#'\item ncols: The number of columns in the data frame
#'\item filePath: A character denoting the path to the h5 file
#'\item nfactors: The number of factor columns in the h5DF object
#'\item factors: A list containing the factor levels for each factor in the data frame
#'\item chunkSize: The number of rows each chunk will contain
#'\item MAT: For internal use when manipulating the data frame
#'\item oldNChunks: For internal use.
#'}
h5DF <- setRefClass(
  'h5DF',
  fields = c(nChunks = 'numeric', chunkNames = 'character', 
             colNames = 'character', colClasses = 'character',
             nrows = 'numeric', ncols = 'numeric', filePath = 'character', 
             nfactors = 'numeric',
             factors = 'list', chunkSize = 'numeric', MAT = 'matrix',
             oldNChunks = "numeric")
)

#------------------------------------------------------------------------------------------------
# h5MAT class specification
#------------------------------------------------------------------------------------------------

#' @title Specification of the h5MAT reference class
#' 
#' @description The h5MAT object is an interface to a special type of HDF5 file that holds data
#' for large matrices in chunks. It is designed to allow fast access to chunks of data from the HDF5 file.
#' 
#' @section Methods in the h5MAT object:
#' \itemize{
#' \item h5MAT$new() creates a new h5MAT object, users should use ?newH5MAT() or ?openH5MAT().
#' \item h5MAT$createH5MAT(MAT, filePath, chunkSize, ...) populates a h5MAT object with data from MAT.
#' Users should use ?newH5MAT() or ?openH5MAT().
#' \itemize{
#'    \item MAT a matrix or a path to a csv file containing a matrix to be converted to a h5MAT object
#'    \item filePath a character denoting a path to a location where the h5MAT object wll be written
#'    \item chunkSize number of rows denoting the size of each matrix chunk
#'    \item ... parameters to be passed to the read.csv() function
#' }
#' \item h5MAT$append(MAT, ...) appends matrix MAT to the current h5MAT object
#' \itemize{
#'    \item MAT a matrix or a path to a csv file containing a matrix to be converted to a h5MAT object
#'    \item ... parameters to be passed to the read.csv() function
#' }
#' \item h5MAT$readChunk(chunkName) reads the chunk denotes by chunkName into R as a matrix
#' \itemize{
#'    \item chunkName a character denoting the name of the chunk to be returned as a matrix 
#' }
#' \item h5MAT$readChunks(chunks) read the chunks denotes by chunks into R as a matrix
#' \itemize{
#'    \item chunks a character vector denoting the chunks to be read into R as a single matrix
#' }
#' \item h5MAT$readTable() reads the whole matrix in the h5MAT into R as a matrix
#' \item h5DF$memorize() this function converts the h5MAT object to a h5MEMMAT object. It reads each chunk
#' to memory and supplies pointers in R to access each chunk. Very useful when you have lots of memory
#' in your system and need an efficient way to access chunks of data.
#' }
#' @section Fields in the h5MAT object:
#' These are the fields in the h5MAT object, they are not to be directly modified by the user but can 
#' be accessed by h5MAT$fieldName.
#'
#'\itemize{
#'\item nChunks: The number of chunks in the h5MAT object
#'\item chunkNames: The names of the chunks in the h5MAT object
#'\item nrows: The number of rows in the data frame
#'\item ncols: The number of columns in the data frame
#'\item filePath: A character denoting the path to the h5 file
#'\item chunkSize: The number of rows each chunk will contain
#'\item MAT: For internal use when manipulating the data frame
#'\item oldNChunks: For internal use.
#'}
h5MAT <- setRefClass(
  'h5MAT',
  fields = c(nChunks = 'numeric', chunkNames = 'character',
             nrows = 'numeric', ncols = 'numeric', filePath = 'character', 
             chunkSize = 'numeric', MAT = 'matrix',
             oldNChunks = "numeric")
)

#------------------------------------------------------------------------------------------------
# h5MEMDF class specification
#------------------------------------------------------------------------------------------------

#' @title Specification of the h5MEMDF reference class
#' 
#' @description The h5MEMDF class allows the user to create a h5DF object in memory rather than on file.
#' The data frame chunks are stored in memory and these are accessed by pointers in the h5MEMDF object.
#' 
#' @section Methods in the h5MEMDF object:
#' \itemize{
#'  \item h5MEMDF$new() creates a new h5MEMDF object. Users should use the functions ?newH5MEMDF().
#'  \item h5MEMDF$createH5DF(df, chunkSize, ...) populates a h5MEMDF object with data from
#' a data frame or csv file containing a data frame df. It writes the meta data to the h5MEMDF object
#' and the dataset to a h5 file. Users should use the functions ?newH5MEMDF().
#' \itemize{
#'          \item df a data frame or path to a csv file containing a data frame
#'          \item chunkSize the number of rows that will be written per chunk
#'          \item ... arguments that will be passed to the read.csv() function
#' }
#' \item h5MEMDF$append(df, ...) appends a data frame to the current h5 file and updates the meta data
#' in the file and on the object
#' \itemize{
#'          \item df a data frame or path to a csv file containing a data frame
#'          \item ... arguments that will be passed to the read.csv() function
#' }
#' \item h5MEMDF$readChunk(chunkNum) reads chunkNum from the h5MEMDF object returning a data frame chunk.
#' \itemize{
#'          \item chunkNum numeric number of the chunk to be returned
#' }
#' \item h5MEMDF$createH5DF(filePath) creates a h5DF object from the h5MEMDF object writing the data to file
#' provided by filePath.
#' \itemize{
#'          \item filePath character denoting location for where the h5 file should be written.
#' }
#' }
#' 
#' @section Fields in the h5MEMDF object:
#' These are the fields in the h5MEMDF object, they are not to be directly modified by the user but can 
#' be accessed by h5MEMDF$fieldName.
#'
#'\itemize{
#'\item nChunks: The number of chunks in the h5MEMDF object
#'\item ptrs: A list of pointers to each of the chunks in the data frame
#'\item colNames: The column names in the submitted data frame
#'\item colClasses: The classes of the submitted data frame
#'\item nrows: The number of rows in the data frame
#'\item ncols: The number of columns in the data frame
#'\item nfactors: The number of factor columns in the h5MEMDF object
#'\item factors: A list containing the factor levels for each factor in the data frame
#'\item chunkSize: The number of rows each chunk will contain
#'\item DF: For internal use when manipulating the data frame
#'\item oldNChunks: For internal use.
#' }
h5MEMDF <- setRefClass(
  'h5MEMDF',
  fields = c(nChunks = 'numeric', colNames = 'character', colClasses = 'character',
             nrows = 'numeric', ncols = 'numeric',
             nfactors = 'numeric', ptrs = "list",
             factors = 'list', chunkSize = 'numeric', DF = 'data.frame',
             oldNChunks = "numeric")
)

#------------------------------------------------------------------------------------------------
# h5MEMMAT class specification
#------------------------------------------------------------------------------------------------

#' @title Specification of the h5MEMMAT reference class
#' 
#' @description The h5MEMMAT class allows the user to create a h5MAT object in memory rather than on file.
#' The matrix chunks are stored in memory and these are accessed by pointers in the h5MEMMAT object.
#' 
#' @section Methods in the h5MEMMAT object:
#' \itemize{
#' \item h5MEMMAT$new() creates a new h5MEMMAT object, users should use ?newH5MAT() or ?openH5MAT().
#' \item h5MEMMAT$createH5MAT(MAT, filePath, chunkSize, ...) populates a h5MEMMAT object with data from MAT.
#' Users should use ?newH5MAT() or ?openH5MAT().
#' \itemize{
#'    \item MAT a matrix or a path to a csv file containing a matrix to be converted to a h5MEMMAT object
#'    \item filePath a character denoting a path to a location where the h5MEMMAT object wll be written
#'    \item chunkSize number of rows denoting the size of each matrix chunk
#'    \item ... parameters to be passed to the read.csv() function
#' }
#' \item h5MEMMAT$append(MAT, ...) appends matrix MAT to the current h5MEMMAT object
#' \itemize{
#'    \item MAT a matrix or a path to a csv file containing a matrix to be converted to a h5MEMMAT object
#'    \item ... parameters to be passed to the read.csv() function
#' }
#' \item h5MEMMAT$readChunk(chunkName) reads the chunk denotes by chunkName into R as a matrix
#' \itemize{
#'    \item chunkName a character denoting the name of the chunk to be returned as a matrix 
#' }
#' \item h5MEMMAT$readChunks(chunks) read the chunks denotes by chunks into R as a matrix
#' \itemize{
#'    \item chunks a character vector denoting the chunks to be read into R as a single matrix
#' }
#' \item h5MEMMAT$readTable() reads the whole matrix in the h5MEMMAT into R as a matrix
#' \item h5MEMMAT$createH5MAT(filePath) this function converts the h5MEMMAT object to a h5MAT object. 
#' It writes the data in the h5MEMMAT object to filePath and returns a h5MAT object.
#' }
#' @section Fields in the h5MEMMAT object:
#' These are the fields in the h5MEMMAT object, they are not to be directly modified by the user but can 
#' be accessed by h5MEMMAT$fieldName.
#'
#'\itemize{
#'\item nChunks: The number of chunks in the h5MEMMAT object
#'\item ptrs: A list of pointers to each of the chunks in the matrix
#'\item nrows: The number of rows in the data frame
#'\item ncols: The number of columns in the data frame
#'\item chunkSize: The number of rows each chunk will contain
#'\item MAT: For internal use when manipulating the data frame
#'\item oldNChunks: For internal use.
#'}
h5MEMMAT <- setRefClass(
  'h5MEMMAT',
  fields = c(nChunks = 'numeric', nrows = 'numeric', ncols = 'numeric', ptrs = "list", 
             chunkSize = 'numeric', MAT = 'matrix', oldNChunks = "numeric")
)
#------------------------------------------------------------------------------------------------
