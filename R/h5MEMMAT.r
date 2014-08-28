# h5MEMMAT Methods
#------------------------------------------------------------------------------------------------
# Helper Methods
#------------------------------------------------------------------------------------------------

# Method: createIndicies()
h5MEMMAT$methods(createIndicies = function(){
  ' The createIndicies() methods creates the indices that will be used to chunk the data
    Usage:
    $createIndicies()
  '
  nRows <- nrow(MAT)
  stubSize <- nRows %% chunkSize
  # Creating the indicies
  if(stubSize != 0)
  {
    index <- c(1, seq(stubSize, nRows, by = chunkSize) + 1)
    indicies <- list()
    for(i in 2:length(index)){
      indicies[[i-1]] <- c(index[i - 1], index[i] - 1)
    }
  }else{
    index <- seq(chunkSize, nRows, chunkSize)
    indicies <- list()
    for(i in 1:length(index))
    {
      indicies[[i]] <- c(index[i] - chunkSize + 1, index[i])
    }
  }
  return(indicies)
})

#------------------------------------------------------------------------------------------------
# Write Methods
#------------------------------------------------------------------------------------------------

# Method: createMetaData()
h5MEMMAT$methods(createMetaData = function(){
  ' The createMetaData() method is not intended for the user. It is used to create the
    meta data for a new object in the h5MEMMAT object.
    Usage:
    $createMetaData()
  '
  
  nrows <<- nrow(MAT)
  ncols <<- ncol(MAT)
  
  stub  <- nrows %% chunkSize
  if(stub == 0){
    nChunks <<- nrows %/% chunkSize
  }else{
    nChunks <<- nrows %/% chunkSize + 1
  }
  
  oldNChunks <<- 0
})

# Method: writeMatrix()
h5MEMMAT$methods(writeMatrix = function(){
  ' The method writeMatrix() is not intended for the user. It is an internal method to be used to 
    write chunks of data as matrices to the h5 object.
    Usage:
    $writeMatrix()
  '
  indicies <- createIndicies()
  cat("Writing to memory", "\n")
  for(i in 1:length(indicies)){
    ind <- indicies[[i]]
    subMAT <- MAT[seq(ind[1], ind[2]),]
    ptrs <<- c(ptrs, h5WriteMemMAT(obj = subMAT))
  }
})

# Method: createH5MEMMAT()
h5MEMMAT$methods(createH5MEMMAT = function(MAT, chunkSize, ...){
  ' The createH5MEMMAT() method populates a new h5MAT object with data defined by MAT, filePath, and chunkSize.
    Users are directed to the newH5MEMMAT() function which are more convenient.
    @param MAT matrix or character file path to CSV containing matrix to be written to H5 file object
    @param chunkSize the number of rows the defines each chunk
    @param ... arguments passed to read.csv() function
    Usage:
    $createH5MEMMAT((matrix or character) MAT, (numeric) chunkSize, ...)
  '
  cat("Initializing ...\n")
  if(class(MAT) == "character"){
    if(file.exists(MAT))
    {
      cat("Reading data from CSV file", "\n")
      mat <- read.csv(file = MAT, ...)
      MAT <<- do.call(cbind, lapply(1:ncol(mat), function(i)mat[,i]))
    }else{
      stop("MAT not recognised as text file or data frame")
    }
  }else{
    MAT <<- MAT
  }
  chunkSize <<- chunkSize
  createMetaData()
  writeMatrix()
  MAT <<- matrix(0)
})

#' @title Function for instantiating a h5MEMMAT object
#' 
#' @param MAT the matrix or path to a csv file containing a matrix to be written to a h5MEMMAT object
#' @param chunkSize the number of rows in each chunk
#' @param ... arguments passed to read.csv() file
#' @return a h5MEMMAT object
#' @examples
#' cat(getwd(), "\n")
#' n <- 1e3
#' mat <- matrix(runif(n^2), nc = n)
#' hmat <- newH5MEMMAT(mat, 10) # creating the h5MEMMAT object
#' hmat$append(mat, 10) # appending to the h5MEMMAT object
#' hmat$readChunk(2)[,1:6] # reading the second chunk
#' # Writing a h5MAT object to file
#' hmat2 <- newH5MAT(mat, "mat.h5", 10)
#' hmat2 # this is a h5MAT object
#' # Reading the matrix to memory
#' hmat3 <- hmat2$memorize()
newH5MEMMAT <- function(MAT, chunkSize, ...)
{
  obj <- h5MEMMAT$new()
  obj$createH5MEMMAT(MAT, chunkSize, ...)
  return(obj)
}

#------------------------------------------------------------------------------------------------
# Update Methods
#------------------------------------------------------------------------------------------------

# Matrix: updateMetaData()
h5MEMMAT$methods(updateMetaData = function(){
  ' The updateMetaData() method is an internal method and not intended to use by the user. It updates
    the meta data of the h5MEMMAT object on file and in R when the user appends new data.
    Usage:
    $updateMetaData()
  '
  nRows <- nrow(MAT)
  nrows <<- nrows + nRows
  oldNChunks <<- nChunks
  # Updating nChunks
  stub  <- nRows %% chunkSize
  if(stub == 0){
    nchunks <- nRows %/% chunkSize
  }else{
    nchunks <- nRows %/% chunkSize + 1
  }
  nChunks <<- oldNChunks + nchunks
  newChunkNames <- paste("ch", (oldNChunks + 1):nChunks, sep = "")
})

# Method: append()
h5MEMMAT$methods(append = function(MAT, ...){
  ' The append() method allows new data to be appended to a h5MEMMAT object.
    @param MAT the matrix to be appended. Either a matrix or a character denoting a csv file
          containing the matrix to be appended.
    @param ... parameters passed to read.csv() function
    Usage:
    $append((matrix or character) MAT, (passed to read.csv()) ...)
  '
  if(class(MAT) == "character"){
    if(file.exists(MAT))
    {
      cat("Reading data from CSV file", "\n")
      mat <- read.csv(file = MAT, ...)
      MAT <<- do.call(cbind, lapply(1:ncol(mat), function(i)mat[,i]))
    }else{
      stop("MAT not recognised as text file or data frame")
    }
  }else{
    MAT <<- MAT
  }
  updateMetaData()
  writeMatrix()
  oldNChunks <<- 0
  MAT <<- matrix(0)
})

#------------------------------------------------------------------------------------------------
# Read Methods
#------------------------------------------------------------------------------------------------

# Method: readChunk()
h5MEMMAT$methods(readChunk = function(chunkNum){
  ' The readChunk() method reads a chunk from the h5MEMMAT object into R as a matrix
    @param chunkNum the number of the chunk to be read into R.
    Usage:
    $readChunk((numeric) chunkNum)
  '
  return(h5ReadMemMAT(ptrs[[chunkNum]]))
})

# Method: readChunks()
h5MEMMAT$methods(readChunks = function(chunkNums){
  ' The readChunks() methods reads chunks defined by a numeric vector into R
    @param chunkNums a numeric vector of chunks to be read into R.
    Usage:
    $readChunks((numeric) chunkNums)
  '
  return(do.call(rbind, lapply(ptrs[chunkNums], h5ReadMemMAT)))
})

# Method: readTable()
h5MEMMAT$methods(readTable = function(){
  ' The readTable() method reads all the chunks in the h5MEMMAT object and returns them as a single matrix.
    Usage:
    $readTable()
  '
  return(do.call(rbind, lapply(ptrs, h5ReadMemMAT)))
})

#------------------------------------------------------------------------------------------------
# Write to H5 file
#------------------------------------------------------------------------------------------------

# Method: createH5MAT()
h5MEMMAT$methods(createH5MAT = function(filePath){
  ' The createH5MAT() method writes the memory matrix to file and returns a h5MAT object
    @param filePath character for the path to the file where the h5 object will be written
    Usage:
    $createH5MAT((character) filePath)
  '
  h5CreateFile(filePath, overwrite = 1)
  cat("Creating the meta data on the h5MAT object", "\n")
  newObj <- h5MAT$new()
  newObj$nChunks <- nChunks
  newObj$nrows <- nrows
  newObj$ncols <- ncols
  newObj$filePath <- filePath
  newObj$chunkSize <- chunkSize
  newObj$oldNChunks <- oldNChunks
  
  chunkNames = paste("ch", 1:nChunks, sep = "")
  newObj$chunkNames <- chunkNames
  
  cat("Writing the meta data to file", "\n")
  # Creating the meta data on the file
  # Initialize the structure for the meta data group
  h5CreateMetaData(filePath)
  h5WriteInt("NRow", nrows, filePath, update = 0)
  h5WriteInt("NCol", ncols, filePath, update = 0)
  h5WriteInt("NChunks", nChunks, filePath, update = 0)
  h5WriteInt("ChunkSize", chunkSize, filePath, update = 0)
  h5WriteInt("Finalized", 0, filePath, update = 0)
  h5WriteCharVector("ChunkNames", chunkNames, filePath, update = 0)
  
  cat("Writing the matrix chunks to file", "\n")
  # Write the matrix chunks to the file
  for(i in 1:nChunks)
  {
    subMatrix <- readChunk(i)
    h5WriteDoubleMat(dset = chunkNames[i], chunk = subMatrix, dim = dim(subMatrix), filePath)
  }
  cat("h5MEMMAT written to file", "\n")
  return(newObj)
})

#------------------------------------------------------------------------------------------------
# Methods related to h5MAT
#------------------------------------------------------------------------------------------------

# Method: memorize()
h5MAT$methods(memorize = function(){
  ' The memorize() method writes a h5MAT object to memory and returns a h5MEMMAT object
    Usage:
    $memorize()
  '
  cat("Writing all the chunks to memory", "\n")
  ptrs <- list()
  for(i in chunkNames){
    ptrs <- c(ptrs, h5WriteMemMAT(readChunk(i)))
  }
  cat("Chunks written to memory", "\n")
  newObj <- h5MEMMAT$new()
  newObj$ptrs <- ptrs
  newObj$nChunks <- nChunks
  newObj$chunkSize <- chunkSize
  newObj$nrows <- nrows
  newObj$ncols <- ncols
  newObj$oldNChunks <- 0
  return(newObj)
})

