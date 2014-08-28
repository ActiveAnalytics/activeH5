# h5MAT Methods
#------------------------------------------------------------------------------------------------
# Helper Methods
#------------------------------------------------------------------------------------------------

# Method: close()
h5MAT$methods(close = function(){
  ' The close() method closes the h5 file object
    Usage:
    $close()
  '
  h5CloseFile(filePath)
})


# Method: flush()
h5MAT$methods(flush = function(){
  ' The flush() method is the same as the close method
    Usage:
    $flush()
  '
  h5FlushFile(filePath)
})

#------------------------------------------------------------------------------------------------

# Method: createIndicies()
h5MAT$methods(createIndicies = function(){
  ' The createIndicies() methods creates the indices that will be used to chunk the data
    Usage:
    $createIndicies()
  '
  nRows <- nrow(MAT)
  stubSize <- nRows %% chunkSize
  # Creating the indicies
  if(stubSize != 0)
  {
    #nChunks <- nRows %/% chunkSize + 1
    index <- c(1, seq(stubSize, nRows, by = chunkSize) + 1)
    indicies <- list()
    for(i in 2:length(index)){
      indicies[[i-1]] <- c(index[i - 1], index[i] - 1)
    }
  }else{
    #nChunks <- nRows / chunkSize
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
h5MAT$methods(createMetaData = function(){
  ' The createMetaData() method is not intended for the user. It is used to create the
    meta data for a new object on file and in the h5MAT object.
    Usage:
    $createMetaData()
  '
  nrows <<- nrow(MAT) # Time efficient
  ncols <<- ncol(MAT) # Time efficient
  
  stub  <- nrows %% chunkSize
  if(stub == 0){
    nChunks <<- nrows %/% chunkSize
  }else{
    nChunks <<- nrows %/% chunkSize + 1
  }
  chunkNames <<- paste("ch", 1:nChunks, sep = "")
  
  cat('Creating meta data on file ...', '\n')
  
  oldNChunks <<- 0
  
  # Create the H5 file
  h5CreateFile(filePath, overwrite = 1)
  # Initialize the structure for the meta data group
  h5CreateMetaData(filePath)
  # Write the number of rows
  h5WriteInt("NRow", nrows, filePath, update = 0)
  # Write the number of columns
  h5WriteInt("NCol", ncols, filePath, update = 0)
  # Number of chunks
  h5WriteInt("NChunks", nChunks, filePath, update = 0)
  # Chunk Size
  h5WriteInt("ChunkSize", chunkSize, filePath, update = 0)
  # Chunk Names
  h5WriteCharVector("ChunkNames", chunkNames, filePath, update = 0)
#   h5WriteInt("Finalized", 0, filePath, update = 0)
})

# Method: writeDoubleMat()
h5MAT$methods(writeDoubleMat = function(){
  ' The method writeDoubleMat() is not intended for the user. It is an internal method to be used to 
    write chunks of data as matrices to the h5 object.
    Usage:
    $writeDoubleMat()
  '
  indicies <- createIndicies()
  for(i in 1:length(indicies)){
    ind <- indicies[[i]]
    subMatrix <- MAT[seq(ind[1], ind[2]),]
    cat("Writing chunk", i, "\n")
    h5WriteDoubleMat(dset = chunkNames[(i + oldNChunks)], chunk = subMatrix, dim = dim(subMatrix), filePath)
  }
})

# Method: createH5MAT()
h5MAT$methods(createH5MAT = function(MAT, filePath, chunkSize, ...){
  ' The createH5MAT() method populates a new h5MAT object with data defined by MAT, filePath, and chunkSize.
    Users are directed to the newH5MAT() and openH5MAT() functions which are more convenient.
    @param MAT matrix or character file path to CSV containing matrix to be written to H5 file object
    @param filePath character path to file where h5 file will be written
    @param ... arguments passed to read.csv() function
    Usage:
    $createH5MAT((matrix or character) MAT, (character) filePath, (numeric) chunkSize, ...)
  '
  cat("Initializing ...\n")
  if(class(MAT) == "character"){
    if(file.exists(MAT))
    {
      cat("Reading data from CSV file", "\n")
      mat <- read.csv(file = MAT, ...)
      MAT <<- do.call(cbind, lapply(1:ncol(mat), function(i)mat[,i]))
    }else{
      stop("MAT not recognised as text file or matrix")
    }
  }else{
    MAT <<- MAT
  }
  filePath <<- filePath
  chunkSize <<- chunkSize
  createMetaData()
  writeDoubleMat()
  MAT <<- matrix(0)
})

#' @title Function to create a new h5MAT object
#' 
#' @param MAT matrix or csv file containing a matrix to be converted into a h5MAT object
#' @param filePath character path to the file to be written
#' @param chunkSize the number of rows for each chunk
#' @param ... arguments sent to \code{read.csv()} function
#' @return returns a h5MAT object
#' @examples
#' n <- 1e3
#' mat <- matrix(runif(n^2), nc = n) 
#' hmat <- newH5MAT(mat, "mat.h5", 1E2) # a h5MAT object
#' hmat$append(mat, "mat.h5", 1E2) # append the matrix
#' ch <- hmat$readChunk("ch2") # reading a chunk into the matrix
#' ch[1:10, 1:10]
#' # Reading several chunks
#' hmat$readChunks(paste("ch", 1:5, sep = ""))
#' # Reading the table
#' tab <- hmat$readTable()
#' # Create a h5MEMMAT object
#' memtab <- hmat$memorize()
newH5MAT <- function(MAT, filePath, chunkSize, ...)
{
  obj <- h5MAT$new()
  obj$createH5MAT(MAT, filePath, chunkSize, ...)
  return(obj)
}

#------------------------------------------------------------------------------------------------
# Update Methods
#------------------------------------------------------------------------------------------------

# Method: updateMetaData()
h5MAT$methods(updateMetaData = function(){
  ' The updateMetaData() method is an internal method and not intended to use by the user. It updates
    the meta data of the h5MAT object on file and in R when the user appends new data.
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
  chunkNames <<- c(chunkNames, newChunkNames)
  
  h5WriteInt("NRow", nrows, filePath, update = 1)
  h5WriteInt("NChunks", nChunks, filePath, update = 1)
  h5WriteCharVector("ChunkNames", chunkNames, filePath, update = 1)
})

# Method: append()
h5MAT$methods(append = function(MAT, ...){
  ' The append() method allows new data to be appended to a h5MAT object.
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
      stop("MAT not recognised as text file or matrix")
    }
  }else{
    MAT <<- MAT
  }
  updateMetaData()
  writeDoubleMat()
  oldNChunks <<- 0
  MAT <<- matrix(0)
})

#------------------------------------------------------------------------------------------------
# Read Methods
#------------------------------------------------------------------------------------------------

#' @title Function to create a h5MAT object from a saved h5 file
#' 
#' @param filePath path to file containing h5MAT object
#' @return returns a h5MAT object
#' @examples
#' n <- 1e3
#' mat <- matrix(runif(n^2), nc = n) 
#' hmat <- newH5MAT(mat, "mat.h5", 1E2) # a h5MAT object
#' hmat2 <- openH5MAT("mat.h5")
openH5MAT <- function(filePath){
  if(!file.exists(filePath)){
    stop("File does not exists")
  }
  cat("Reading in basic file information", "\n")
  nChunks <- h5ReadInt(intName = "NChunks", filePath)
  chunkNames <- h5ReadCharVector(charName = "ChunkNames", filePath)
  nrows <- h5ReadInt(intName = "NRow", filePath)
  ncols <- h5ReadInt(intName = "NCol", filePath)
  chunkSize <- h5ReadInt(intName = "ChunkSize", filePath)
  
  cat("Writing items to local h5MAT object")
#   finalized <- h5ReadInt(intName = "Finalized", filePath)
#   if(finalized == 0){
#     eMess <- "The file you are trying to read is corrupted, it has not been finalized."
#     stop(eMess)
#   }
  
  # Creating and updating the h5MAT object
  h5Obj <- h5MAT$new()
  h5Obj$filePath <- filePath
  h5Obj$nrows <- nrows
  h5Obj$ncols <- ncols
  h5Obj$nChunks <- nChunks
  h5Obj$chunkNames <- chunkNames
  h5Obj$chunkSize <- chunkSize
  h5Obj$oldNChunks <- 0
  return(h5Obj)
}

# Method: readChunk()
h5MAT$methods(readChunk = function(chunkName){
  ' The readChunk() method reads a chunk from the h5MAT object into R as a matrix
    @param chunkName the name of the chunk to be read into R.
    Usage:
    $readChunk((character) chunkName)
  '
  cat("Reading chunk ", chunkName, "\n")
  df <- h5ReadDoubleMat(chunkName, filePath)
  return(df)
})

# Method: readChunks()
h5MAT$methods(readChunks = function(chunks){
  ' The readChunks() methods reads chunks defined by a character vector into R
    @param chunks a character vector of chunkNames to be read into R.
    Usage:
    $readChunks((character) chunks)
  '
  cat("Reading the data from H5 file ...", "\n")
  df <- do.call(rbind, lapply(chunks, h5ReadDoubleMat, filePath = filePath))
  return(df)
})


# Method: readTable()
h5MAT$methods(readTable = function(){
  ' The readTable() method reads all the chunks in the h5MAT object and returns them as a single matrix.
    Usage:
    $readTable()
  '
  cat("Reading the data from H5 file ...", "\n")
  df <- do.call(rbind, lapply(chunkNames, h5ReadDoubleMat, filePath = filePath))
  return(df)
})

