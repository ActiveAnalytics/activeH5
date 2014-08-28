# h5DF Methods
#------------------------------------------------------------------------------------------------
# Helper Methods
#------------------------------------------------------------------------------------------------

# Method: close()
h5DF$methods(close = function(){
  'The close() method closes the h5 file
    Usage:
    $close()
  '
  h5CloseFile(filePath)
})

# Method: flush()
h5DF$methods(flush = function(){
  ' The flush() method does the same thing as the close method
    Usage:
    $flush()
  '
  h5FlushFile(filePath)
})


#------------------------------------------------------------------------------------------------

# Method: createIndicies()
h5DF$methods(createIndicies = function(){
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
h5DF$methods(createMetaData = function(DF){
  ' The createMetaData() method is not intended for the user. It is used to create the
    meta data for a new object on file and in the h5DF object.
    @param DF data frame to be used to create the meta data
    Usage:
    $createMetaData((data.frame) DF)
  '
  
  nrows <<- nrow(DF)
  ncols <<- ncol(DF)
  
  stub  <- nrows %% chunkSize
  if(stub == 0){
    nChunks <<- nrows %/% chunkSize
  }else{
    nChunks <<- nrows %/% chunkSize + 1
  }
  chunkNames <<- paste("ch", 1:nChunks, sep = "")
  
  DF <- as.list(DF)
  colNames <<- names(DF)
  colClasses <<- sapply(DF, class)
  
  cat("Converting character to factors", "\n")
  colClasses[colClasses == "character"] <<- "factor"
  cat("Registering any factor columns", "\n")
  factorCols <- colNames[which(colClasses == 'factor')]
  nfactors <<- length(factorCols)
  if(nfactors > 0){
    lFactors <- vector(mode = 'list', length = nfactors)
    names(lFactors) <- factorCols
    for(i in factorCols)
    {
      colClass <- class(DF[[i]][1])
      if(colClass != "factor")
      {
        DF[[i]] <- factor(DF[[i]])
      }
      lFactors[[i]] <- levels(DF[[i]][1])
    }
    factors <<- lFactors
  }
  oldNChunks <<- 0
  
  cat("Creating the matrix for writing ...", "\n")
  DF <- lapply(DF, unclass)
  DF <- do.call(cbind, DF)
  DF[1,1] <- as.numeric(DF[1,1])
  MAT <<- DF
  
  h5CreateFile(filePath, overwrite = 1)
  h5CreateMetaData(filePath);
  h5WriteInt("NRow", nrows, filePath, update = 0)
  h5WriteInt("NCol", ncols, filePath, update = 0)
  h5WriteInt("NChunks", nChunks, filePath, update = 0)
  h5WriteInt("ChunkSize", chunkSize, filePath, update = 0)
  h5WriteCharVector("ColumnNames", colNames, filePath, update = 0)
  h5WriteCharVector("ColumnClasses", colClasses, filePath, update = 0)
  h5WriteCharVector("ChunkNames", chunkNames, filePath, update = 0)
  if(nfactors > 0){
    for(i in factorCols)
    {
      h5WriteFactor(i, factors[[i]], filePath, update = 0)
    }
  }
})

# Method: writeDoubleMat()
h5DF$methods(writeDoubleMat = function(){
  ' The method writeDoubleMat() is not intended for the user. It is an internal method to be used to 
    write chunks of data as matrices to the h5 object.
    Usage:
    $writeDoubleMat()
  '
  indicies <- createIndicies()
  for(i in 1:length(indicies)){
    ind <- indicies[[i]]
    subMatrix <- MAT[seq(ind[1], ind[2]),]
    h5WriteDoubleMat(dset = chunkNames[(i + oldNChunks)], chunk = subMatrix, dim = dim(subMatrix), filePath)
  }
})

# Method: createH5DF()
h5DF$methods(createH5DF = function(DF, filePath, chunkSize, ...){
  ' The createH5DF() method populates a new h5DF object with data defined by DF, filePath, and chunkSize.
    Users are directed to the newH5DF() and openH5DF() functions which are more convenient
    @param DF the data frame to be appended. Either a data.frame or a character denoting a csv file
          containing the data frame to be appended.
    @param filePath path to the file where the h5 object will be written
    @param chunkSize number of rows in each chunk
    @param ... parameters passed to read.csv() function
    Usage:
    $createH5DF((data.frame or character) DF, (character) filePath, (numeric) chunkSize, ...)
  '
  cat("Initializing ...\n")
  
  if(class(DF) == "character"){
    if(file.exists(DF))
    {
      cat("Reading data from CSV file", "\n")
      DF <- read.csv(file = DF, ...)
    }else{
      stop("DF not recognised as text file or data frame")
    }
  }
  
  filePath <<- filePath
  chunkSize <<- chunkSize
  cat('Creating meta data on file ...', '\n')
  createMetaData(DF)
  cat('Writing data to file ...', '\n')
  writeDoubleMat()
  MAT <<- matrix(0)
})

#' @title Function to create a new h5DF object
#' 
#' @param DF data frame or file path to csv file containing data frame to be written to h5DF object
#' @param filePath path to file that will contain the h5DF data object
#' @param chunkSize the number of rows that each chunk will take
#' @param ... parameters passed to read.csv() function
#' @return a h5DF object
#' @examples
#' # Create a new h5DF object from the iris dataset
#' ir1 <- newH5DF(iris, "iris.h5", 10)
#' ir1 # The details of the object
#' chNames <- ir1$chunkNames # The names of the chunks
#' ir1$readChunk(chNames[3]) # read chunk number 3
#' ir1$readChunks(chNames[1:5]) # read the first 5 chunks
#' ir1$readTable() # reads back the whole table
#' ir2 <- openH5DF("iris.h5") # create H5 df from file
#' # Create a h5MEMDF object from a h5DF object by memorize (i.e. brings the h5DF object into memory)
#' irMDF <- ir2$memorize()
#' irMDF # this is a h5MEMDF object, chunked DF object in memory with pointers
#' irMDF$ptrs
newH5DF <- function(DF, filePath, chunkSize = 50000, ...)
{
  obj <- h5DF$new()
  obj$createH5DF(DF, filePath, chunkSize, ...)
  return(obj)
}

#------------------------------------------------------------------------------------------------
# Update Methods
#------------------------------------------------------------------------------------------------

# Method: updateMetaData()
h5DF$methods(updateMetaData = function(DF){
  ' The updateMetaData() method is an internal method and not intended to use by the user. It updates
    the meta data of the h5DF object on file and in R when the user appends new data.
    @param DF
    Usage:
    $updateMetaData((data.frame) DF)
  '
  nRows <- nrow(DF)
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
  #print(newChunkNames)
  chunkNames <<- c(chunkNames, newChunkNames)
  
  # Convert DF to list
  DF <- as.list(DF)
  
  # Column class adjustments
  factorCols <- colNames[colClasses == 'factor']
  lFactors <- factors # local copy of factors
  for(i in factorCols)
  {
    colClass <- class(DF[[i]][1])
    if(colClass == "factor")
    {
      lLvls <- lFactors[[i]]
      tmpLvls <- levels(DF[[i]])
      lFactors[[i]] <- c(lLvls, tmpLvls[!(tmpLvls %in% lLvls)])
      DF[[i]] <- factor(DF[[i]], levels = lFactors[[i]])
    }
    if(colClass != "factor")
    {
      warning("Converting non factor columns to factors ...")
      lLvls <- lFactors[[i]]
      tmpUnique <- as.character(unique(DF[[i]]))
      lFactors[[i]] <- c(lLvls, tmpUnique[!(tmpUnique %in% lLvls)])
      DF[[i]] <- factor(DF[[i]], levels = lFactors[[i]])
    }
  }
  
  factors <<- lFactors
  # Conversion to numeric (may not have to do this, cbind may simple enforce numeric)
  DF <- lapply(DF, unclass)
  DF <- do.call(cbind, DF)
  # Enforcing numeric matric
  DF[1,1] <- as.numeric(DF[1,1])
  # Writing to MAT field
  MAT <<- DF
  
  # Updating the meta data
  # Update the number of rows
  h5WriteInt("NRow", nrows, filePath, update = 1)
  # Number of chunks
  h5WriteInt("NChunks", nChunks, filePath, update = 1)
  # char vector to update chunk names
  h5WriteCharVector("ChunkNames", chunkNames, filePath, update = 1)
  # Now write the factors into the meta data if they are present
  if(nfactors > 0){
    for(i in factorCols)
    {
      # Writing factors to the factor group of the metadata group of the H5 file
      h5WriteFactor(i, lFactors[[i]], filePath, update = 1)
    }
  }
  # Set Finalized
  #h5WriteInt("Finalized", 0, filePath, update = 1)
})

# Method: append()
h5DF$methods(append = function(DF, ...){
  ' The append() method allows new data to be appended to a h5DF object.
    @param DF the data frame to be appended. Either a data.frame or a character denoting a csv file
          containing the data frame to be appended.
    @param ... parameters passed to read.csv() function
    Usage:
    $append((data.frame or character) DF, (passed to read.csv) ...)
  '
  if(class(DF) == "character"){
    if(file.exists(DF))
    {
      cat("Reading data from CSV file", "\n")
      DF <- read.csv(file = DF, ...)
    }else{
      stop("DF not recognised as text file or data frame")
    }
  }
  cat('Updating meta data on file ...', '\n')
  updateMetaData(DF)
  cat('Appending data to file ...', '\n')
  writeDoubleMat()
  oldNChunks <<- 0
  MAT <<- matrix(0)
})

#------------------------------------------------------------------------------------------------
# Read Methods
#------------------------------------------------------------------------------------------------

#' @title Function to create a new h5DF object from h5 file
#' 
#' @param filePath path to file that contain the h5DF data object
#' @return a h5DF object
#' @examples
#' # Create a new h5DF object from the iris dataset
#' ir1 <- newH5DF(iris, "iris.h5", 10)
#' ir1 # The details of the object
#' chNames <- ir1$chunkNames # The names of the chunks
#' ir1$readChunk(chNames[3]) # read chunk number 3
#' ir1$readChunks(chNames[1:5]) # read the first 5 chunks
#' ir1$readTable() # reads back the whole table
#' ir2 <- openH5DF("iris.h5") # create H5 df from file
#' # Create a h5MEMDF object from a h5DF object by memorize (i.e. brings the h5DF object into memory)
#' irMDF <- ir2$memorize()
#' irMDF # this is a h5MEMDF object, chunked DF object in memory with pointers
#' irMDF$ptrs
openH5DF <- function(filePath){
  if(!file.exists(filePath)){
    stop("File does not exists")
  }
  # Creating and updating the h5DF object
  h5Obj <- h5DF$new()
  h5Obj$filePath <- filePath
  cat("Reading in basic file information", "\n")
  nChunks <- h5ReadInt(intName = "NChunks", filePath)
  chunkNames <- h5ReadCharVector(charName = "ChunkNames", filePath)
  cat("Checking chunkNames against nChunks")
  
  h5Obj$nChunks <- nChunks
  h5Obj$chunkNames <- chunkNames
  h5Obj$nrows <- h5ReadInt(intName = "NRow", filePath)
  h5Obj$ncols <- h5ReadInt(intName = "NCol", filePath)
  h5Obj$chunkSize <- h5ReadInt(intName = "ChunkSize", filePath)
  colNames <- h5ReadCharVector(charName = "ColumnNames", filePath)
  colClasses <- h5ReadCharVector(charName = "ColumnClasses", filePath)
  
  
  factorNames <- colNames[colClasses == "factor"]
  nfactors <- length(factorNames)
  if(nfactors > 0)
  {
    factors <- vector(mode = "list", length = nfactors)
    names(factors) <- factorNames
    for(i in factorNames)
    {
      factors[[i]] <- h5ReadFactor(i, filePath)
    }
  }else{
    factors <- list()
  }
  h5Obj$colNames <- colNames
  h5Obj$colClasses <- colClasses
  h5Obj$nfactors <- nfactors
  h5Obj$factors <- factors
  h5Obj$oldNChunks <- 0
  return(h5Obj)
}

# Method: readChunk()
h5DF$methods(readChunk = function(chunkName){
  ' The readChunk() method reads a chunk from the h5DF object into R as a data frame
    @param chunkName the name of the chunk to be read into R.
    Usage:
    $readChunk((character) chunkName)
  '
  return(do.call(cbind.data.frame, h5ChunkList(chunkName, filePath)))
})

# Method: chunkSel()
h5DF$methods(chunkSel = function(chunkName, selCols){
  ' The chunkSel method returns a data frame form the h5DF object selecting columns
    specified by selCols.
    @param chunkName character the name of the chunk to be read
    @param selCols character names of the columns to be selecting within the chunk
    $chunkSel((character) chunkName, (character) selCols)
  '
  return(do.call(cbind.data.frame, h5ChunkSel(chunkName, selCols, filePath)))
})

# Method: modelFrame()
h5DF$methods(modelFrame = function(chunkName, selCols){
  ' The modelFrame method returns a list from the h5DF object selecting columns
    specified by selCols, omiting rows that have NA values. Each list item represents
    a column from the data frame.
    @param chunkName character the name of the chunk to be read
    @param selCols character names of the columns to be selecting within the chunk
    $modelFrame((character) chunkName, (character) selCols)
  '
  return(h5ModelFrame(chunkName, selCols, filePath))
})

# Method: readChunks()
h5DF$methods(readChunks = function(chunks){
  ' The readChunks() methods reads chunks defined by a character vector into R
    @param chunks a character vector of chunkNames to be read into R.
    Usage:
    $readChunks((character) chunks)
  '
  cat("Reading the data from H5 file ...", "\n")
  df <- do.call(rbind, lapply(chunks, h5ReadDoubleMat, filePath = filePath))
  cat("H5 data read, now formatting the data ...", "\n")
  df <- lapply(1:ncol(df), function(x)df[,x])
  names(df) <- colNames
  fNames <- names(factors)
  for(i in 1:length(factors)){
    tmpLvls <- factors[[i]]
    tmp <- as.integer(df[[fNames[i]]])
    attr(tmp, "levels") <- tmpLvls
    attr(tmp, "class") <- "factor"
    df[[fNames[i]]] <- tmp
  }
  df <- do.call(cbind.data.frame, df)
  return(df)
})

# Method: readTable()
h5DF$methods(readTable = function(){
  ' The readTable() method reads all the chunks in the h5DF object and returns them aas a single data frame.
  '
  cat("Reading the data from H5 file ...", "\n")
  df <- do.call(rbind, lapply(chunkNames, h5ReadDoubleMat, filePath = filePath))
  cat("H5 data read, now formatting the data ...", "\n")
  df <- lapply(1:ncol(df), function(x)df[,x])
  names(df) <- colNames
  fNames <- names(factors)
  for(i in 1:length(factors)){
    tmpLvls <- factors[[i]]
    tmp <- as.integer(df[[fNames[i]]])
    attr(tmp, "levels") <- tmpLvls
    attr(tmp, "class") <- "factor"
    df[[fNames[i]]] <- tmp
  }
  df <- do.call(cbind.data.frame, df)
  return(df)
})

# Method: readMatChunk()
h5DF$methods(readMatChunk = function(chunkName){
  ' The readMatChunk() function is not intended to be used by the user. It reads back the data frame
  as a numeric matrix in the form that it is stored in h5 file without converting it back into a data frame.
  It is intended for diagnostic purposes.
  @param chunkName the name of the chunk to be read back.
  Usage:
  $readMatChunk((character) chunkName)
  '
  return(h5ReadDoubleMat(chunkName, filePath))
})

#------------------------------------------------------------------------------------------------
# Legacy Methods
#------------------------------------------------------------------------------------------------
# This function returns a list from c++ which is then bound together to get a data frame
# Slower than the default version
h5DF$methods(readChunkcpp = function(chunkName){
  'The readChunkcpp() method is a legacy method and no longer used'
  cat("Reading chunk ", chunkName, "\n")
  df <- h5ReadDoubleMat2(chunkName, filePath)
  df <- do.call(cbind.data.frame, df)
  return(df)
})

# Function returns the list after processing the matrix returned from the HDF5 file
h5DF$methods(readAsList = function(chunkName){
  'The readAsList() method is a legacy method and no longer used'
  cat("Reading chunk ", chunkName, "\n")
  lst <- h5ReadDoubleMat(chunkName, filePath)
  lst <- lapply(1:ncol(lst), function(x)lst[,x])
  names(lst) <- colNames
  fNames <- names(factors)
  for(i in 1:length(factors)){
    tmpLvls <- factors[[i]]
    tmp <- as.integer(lst[[fNames[i]]])
    attr(tmp, "levels") <- tmpLvls
    attr(tmp, "class") <- "factor"
    lst[[fNames[i]]] <- tmp
  }
  return(lst)
})

# Function to read whole table returning as list of data frames
h5DF$methods(readTableList = function(){
  'The readTableList() method is a legacy method and no longer used'
  out <- list()
  for(i in chunkNames)
    out[[i]]  <- readChunk(i)
  return(out)
})
