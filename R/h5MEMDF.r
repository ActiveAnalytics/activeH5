# h5MEMDF Methods
#------------------------------------------------------------------------------------------------
# Helper Methods
#------------------------------------------------------------------------------------------------

# Method: createIndicies()
h5MEMDF$methods(createIndicies = function(){
  ' This methods creates chunking indices from the current data in the DF field
  Usage:
  $createIndicies()
  '
  nRows <- nrow(DF)
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
h5MEMDF$methods(createMetaData = function(df){
  ' The createMetaData() method is not intended for the user. It is used to create the
    meta data for a new object in the h5MEMDF object.
    @param df data frame to be written to memory
    Usage:
    $createMetaData((data.frame) df)
  '
  
  nrows <<- nrow(df)
  ncols <<- ncol(df)
  
  stub  <- nrows %% chunkSize
  if(stub == 0){
    nChunks <<- nrows %/% chunkSize
  }else{
    nChunks <<- nrows %/% chunkSize + 1
  }
  
  colNames <<- colnames(df)
  colClasses <<- sapply(df, class)
  
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
      colClass <- class(df[[i]][1])
      if(colClass != "factor")
      {
        df[[i]] <- factor(df[[i]])
      }
      lFactors[[i]] <- levels(df[[i]][1])
    }
    factors <<- lFactors
  }
  oldNChunks <<- 0

  DF <<- df
})

h5MEMDF$methods(writeDataFrame = function(){
  ' The writeDataFrame() method writes the data frame to memory and populates the ptrs field in the 
    h5MEMDF object. This function is not intended for the user.
    Usage:
    $writeDataFrame()
  '
  indicies <- createIndicies()
  cat("Writing to memory", "\n")
  for(i in 1:length(indicies)){
    ind <- indicies[[i]]
    subDF <- DF[seq(ind[1], ind[2]),]
    ptrs <<- c(ptrs, h5WriteMemDF(obj = subDF))
  }
})


h5MEMDF$methods(createH5MEMDF = function(df, chunkSize, ...){
  ' The createH5MEMDF() method populates data in memory and the meta data in the h5MEMDF object
  @param df a data frame or path to a csv file containing a data frame
  @param chunkSize the number of rows that each chunk should contain
  @param ... parameters passed to read.csv() function
  Usage:
  $initialize((data.frame or character) df, (character) filePath, (numeric) chunkSize, 
        (parameters passed to read.csv()) ...)
  '
  cat("Initializing ...\n")
  if(class(df) == "character"){
    if(file.exists(df))
    {
      cat("Reading data from CSV file", "\n")
      df <- read.csv(file = df, ...)
    }else{
      stop("DF not recognised as text file or data frame")
    }
  }
  chunkSize <<- chunkSize
  createMetaData(df)
  writeDataFrame()
  DF <<- data.frame(0)
})


#' @title Function to create a new h5MEMDF object
#' 
#' @param df data frame or file path to csv file containing data frame to be written to h5DF object
#' @param chunkSize the number of rows that each chunk will take.
#' @param ... parameters passed to read.csv() function
#' @return a h5MEMDF object
#' @examples
#' # Create a new h5MEMDF object from the iris dataset
#' ir1 <- newH5MEMDF(iris[1:100,], 10)
#' ir1 # The details of the object
#' ir1$readChunk(1) # read chunk number 3
#' ir1$append(iris[101:150,]) # appending data
newH5MEMDF <- function(df, chunkSize, ...)
{
  obj <- h5MEMDF$new()
  obj$createH5MEMDF(df, chunkSize, ...)
  return(obj)
}

#------------------------------------------------------------------------------------------------
# Update Methods
#------------------------------------------------------------------------------------------------

# Method: updateMetaData()
h5MEMDF$methods(updateMetaData = function(df){
  ' The updateMetaData() function updates the meta data in the h5MEMDF object with any new information
    form data to be appended to the current data set.
    @param df data frame or character with path to cssv file containing data frame with data to be used
            to update the h5MEMDF object.
    Usage:
    $updateMetaData((data.frame or character df))
  '
  nRows <- nrow(df)
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
  
  # Column class adjustments
  factorCols <- colNames[colClasses == 'factor']
  lFactors <- factors # local copy of factors
  for(i in factorCols)
  {
    colClass <- class(df[[i]][1])
    if(colClass == "factor")
    {
      lLvls <- lFactors[[i]]
      tmpLvls <- levels(df[[i]][1])
      lFactors[[i]] <- c(lLvls, tmpLvls[!(tmpLvls %in% lLvls)])
      df[[i]] <- factor(df[[i]], levels = lFactors[[i]])
    }
    if(colClass != "factor")
    {
      warning("Converting non factor columns to factors ...")
      lLvls <- lFactors[[i]]
      tmpUnique <- as.character(unique(df[[i]]))
      lFactors[[i]] <- c(lLvls, tmpUnique[!(tmpUnique %in% lLvls)])
      df[[i]] <- factor(df[[i]], levels = lFactors[[i]])
    }
  }
  
  factors <<- lFactors
  DF <<- df
})

# Method: append()
h5MEMDF$methods(append = function(df, ...){
  ' The append() method allows the user to append new data to the current h5MEMDF object.
    @param df the data frame or character path to a csv file containing the data frame to
            be appended to the h5MEMDF object
    @param ... arguments to be passed to read.csv()
    Usage:
    $append((data.frame or character) df, (arguments passed to read.csv()) ...)
  '
  if(class(df) == "character"){
    if(file.exists(df))
    {
      cat("Reading data from CSV file", "\n")
      df <- read.csv(file = df, ...)
    }else{
      stop("DF not recognised as text file or data frame")
    }
  }
  updateMetaData(df)
  writeDataFrame()
  oldNChunks <<- 0
  DF <<- data.frame(0)
})

#------------------------------------------------------------------------------------------------
# Read Methods
#------------------------------------------------------------------------------------------------

# Method: readChunk()
h5MEMDF$methods(readChunk = function(chunkNum){
  ' The readChunk() method reads a chunk denoted by chunkNum from memory into R.
    @param chunkNum numeric number of chunk to be read back into R.
    Usage:
    $readChunk((numeric (chunkNum)))
  '
  return(h5ReadMemDF(ptrs[[chunkNum]]))
})


#------------------------------------------------------------------------------------------------
# Write to H5 file
#------------------------------------------------------------------------------------------------

# Method: createH5DF()
h5MEMDF$methods(createH5DF = function(filePath){
  ' The createH5DF() method writes the memory matrix to file and returns a h5MAT object.
    @param filePath character denoting the path a location where the h5 file can be written.
    Usage:
    $createH5DF((chaaracter) filePath)
  '
  h5CreateFile(filePath, overwrite = 1)
  cat("Creating the meta data on the h5DF object", "\n")
  newObj <- h5DF$new()
  newObj$nChunks <- nChunks
  newObj$nrows <- nrows
  newObj$ncols <- ncols
  newObj$filePath <- filePath
  newObj$chunkSize <- chunkSize
  newObj$oldNChunks <- oldNChunks
  newObj$colNames <- colNames
  newObj$colClasses <- colClasses
  newObj$factors <- factors
  newObj$nfactors <- nfactors
  chunkNames = paste("ch", 1:nChunks, sep = "")
  newObj$chunkNames <- chunkNames
  
  cat("Writing the meta data to file", "\n")
  h5CreateMetaData(filePath)
  h5WriteInt("NRow", nrows, filePath, update = 0)
  h5WriteInt("NCol", ncols, filePath, update = 0)
  h5WriteInt("NChunks", nChunks, filePath, update = 0)
  h5WriteInt("ChunkSize", chunkSize, filePath, update = 0)
  h5WriteCharVector("ColumnNames", colNames, filePath, update = 0)
  h5WriteCharVector("ColumnClasses", colClasses, filePath, update = 0)
  h5WriteCharVector("ChunkNames", chunkNames, filePath, update = 0)
  h5WriteInt("Finalized", 0, filePath, update = 0)
  factorCols <- colNames[which(colClasses == 'factor')]
  # Now write the factors into the meta data if they are present
  if(nfactors > 0){
    for(i in factorCols)
    {
      # Writing factors to the factor group of the metadata group of the H5 file
      h5WriteFactor(i, factors[[i]], filePath, update = 0)
    }
  }
  
  cat("Writing the chunks to file", "\n")
  # Write the matrix chunks to the file
  for(i in 1:nChunks)
  {
    subDF <- readChunk(i)
    subDF <- as.list(subDF)
    subDF <- lapply(subDF, unclass)
    subDF <- do.call(cbind, subDF)
    h5WriteDoubleMat(dset = chunkNames[i], chunk = subDF, dim = dim(subDF), filePath)
  }
  cat("h5MEMDF written to file", "\n")
  return(newObj)
})

# Methods related to h5DF
#------------------------------------------------------------------------------------------------
# Method: memorize()
h5DF$methods(memorize = function(){
  ' The memoize() method takes a h5DF object and puts all DF chunks into memory 
    then returns a h5MEMDF object.
    Usage:
    $memorize()
  '
  cat("Writing all the chunks to memory", "\n")
  ptrs <- list()
  for(i in chunkNames){
    ptrs <- c(ptrs, h5WriteMemDF(readChunk(i)))
  }
  cat("Chunks written to memory", "\n")
  newObj <- h5MEMDF$new()
  newObj$ptrs <- ptrs
  newObj$nChunks <- nChunks
  newObj$chunkSize <- chunkSize
  newObj$colNames <- colNames
  newObj$colClasses <- colClasses
  newObj$nrows <- nrows
  newObj$ncols <- ncols
  newObj$nfactors <- nfactors
  newObj$factors <- factors
  newObj$oldNChunks <- 0
  return(newObj)
})

