/*
  Date: 2014-01-15
  Author: Chibisi Chima-Okereke
  Purpose: C++ backend to HDF5 functionality for h5DF reference class object
*/
// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <iostream>
#include <string>
#include <vector>
#include <H5Cpp.h>
#include <stdlib.h>
#include <fstream>

using namespace Rcpp;
using namespace H5;
using namespace std;


/* This is a worker function to convert SEXP character to a H5std_string */

/* For an SEXP CharacterVector of length 1 */
char* convertChar(SEXP Char)
{
  char *stdString = (char*)CHAR(STRING_ELT(Char, 0));
  //H5std_string StdString(stdString);
  return stdString;
}

/* Overload for an SEXP CharacterVector of any length */
char** convertCharArray(SEXP charVec)
{
  int len = Rf_length(charVec);
  
  // This is the char array
  char** arr = NULL;
  
  // Filling the char* array
  arr = (char**) calloc(len, sizeof(char*));
  for (int i = 0; i < len; i++)
    arr[i] = (char*) CHAR(STRING_ELT(charVec, i));
    
  return arr;
}

/* Check that file exists */
int fileExists(string filePath)
{
  ifstream file(filePath.c_str());
  if(file)
  {
    return 1;
  }
  return 0;
}

IntegerVector cCreateFactor(NumericVector x, CharacterVector levels)
{
  IntegerVector y = (IntegerVector)x;
  y.attr("levels") = levels;
  y.attr("class") = "factor";
  return y;
}

//'@title Function to create a factor
//'
//'@description Function creates a factor when supplied with a numeric vector and character vector for levels
//'
//'@param x a numeric vector that denotes the indexes of the factor
//'@param levels character vector for the unique levels of the factor
//'@return a factor
//'@examples
//'createFactor(sample(1:3, 10, TRUE), LETTERS[1:3])
// [[Rcpp::export]]
SEXP createFactor(NumericVector x, CharacterVector levels)
{
  return wrap(cCreateFactor(x, levels));
}


// Writing to file
/*--------------------------------------------------------------------------------------------------*/

//'@title Function to create file a h5 file
//'
//'@description Function to create a h5 file. It is intended for internal use only
//'
//'@param filePath a character denoting the path to the location where the h5 file will be written
//'@param overwrite integer 1 for overwrite and 0 for not overwrite. Will fail if overwrite is 0
//'@return int 0
// [[Rcpp::export]]
int h5CreateFile(std::string filePath, int overwrite)
{
  H5File* file;
  
  if(fileExists(filePath))
  {
    if(overwrite)
    {
      file = new H5File(filePath, H5F_ACC_TRUNC);
    }else{
      throw "Error: file exists and overwrite is set to 0.";
    }
  }else{
    file = new H5File(filePath, H5F_ACC_TRUNC);
  }
  file->close();
  return 0;
}

//'@title Function creates the groups for the meta data to be written to the h5 file
//'
//'@description Function to create the groups in the h5 file before it is populated.
//'This function is intended for internal use only
//'
//'@param filePath character path to the location where the h5 file will be written
//'@return int 0
// [[Rcpp::export]]
int h5CreateMetaData(std::string filePath)
{
  H5File *file = new H5File(filePath, H5F_ACC_RDWR);
  
  hsize_t dims[1] = {1};  
  // The variable length string type
  StrType vlst(0, H5T_VARIABLE);
  
  // Creating the meta data group
  Group *metaGroup = new Group(file->createGroup("/MetaData"));
  
  // File path
  H5std_string fString("FilePath");
  DataSpace fileDataSpace (1, dims, NULL);
  
  DataSet fileDataSet;
  
  // Create a dataset in the group
  fileDataSet = metaGroup->createDataSet(fString, vlst, fileDataSpace);
  fileDataSet.write(filePath, vlst);
  
  // Create the factor group
  Group *factorGroup = new Group(metaGroup->createGroup("/MetaData/Factor"));
  fileDataSet.close(); //nn
  factorGroup->close();
  metaGroup->close();
  file->close();
  return 0;
}

//'@title This function writes a character vector to the meta data
//'
//'@description This function writes a character vector to the meta data and is intended for internal use.
//'
//'@param charName the name that will be given to the meta data character vector
//'@param charVec the character vector to be written as meta data
//'@param filePath the path to the h5 file where the data will be written
//'@param update integer denoting whether the data item is new or whether it is an update 
//'(which will overwrite any previous item)
//'@return int 0
// [[Rcpp::export]]
int h5WriteCharVector(std::string charName, SEXP charVec, std::string filePath, int update)
{
  H5File *file = new H5File(filePath, H5F_ACC_RDWR);
  
  int len = Rf_length(charVec);
  hsize_t DIM1 = len;
  int rank = 1;
  //cout << "The length is ... " << len << endl;
  // Create a datatype to refer to
  StrType vlst(0, H5T_VARIABLE);
  
  // This is the char array
  char** arr = convertCharArray(charVec);
  
  string meta = "/MetaData";
  
  // Group Meta Group
  Group* metaGroup = new Group(file->openGroup(meta));
  
  // The dataset and dataspace
  hsize_t dims[] = {DIM1};
  //hsize_t maxdims[] = {H5S_UNLIMITED};
  DataSet dataset;
  if(update == 1)
  {
    string slash = "/";
    string groupName = meta + slash + charName;
    file->unlink(groupName); 
  }
  
  DataSpace dataspace(rank, dims);
  dataset = metaGroup->createDataSet(charName, vlst, dataspace);
  dataset.write(arr, vlst);
  dataset.close(); //nn
  metaGroup->close();
  file->close();
  return 0;
}

//'@title This function writes an integer meta data to file
//'
//'@description This function is inteded for internal use
//'
//'@param intName the name of the meta data item to be written
//'@param integer int that will be written to the meta data described by intName
//'@param filePath character path to the h5 file where data will be written
//'@param update int flag for whether item is new (0) or whether it will overwrite a previous item (1)
//'@return int 0
// [[Rcpp::export]]
int h5WriteInt(std::string intName, int integer, std::string filePath, int update)
{
  H5File *file = new H5File(filePath, H5F_ACC_RDWR);
  
  // Colclasses dim
  hsize_t dim[1] = {1};
  
  string meta = "/MetaData";
  // Group Meta Group
  Group* metaGroup = new Group(file->openGroup(meta));
  
  // dataspace
  DataSpace dataspace = DataSpace(1, dim);
  DataSet dataset;
  if(update == 1)
  {
    string slash = "/";
    string groupName = meta + slash + intName;
    file->unlink(groupName);
  }
  dataset = metaGroup->createDataSet(intName, PredType::NATIVE_INT, dataspace);
  dataset.write(&integer, PredType::NATIVE_INT);
  dataset.close(); //nn
  metaGroup->close();
  file->close();
  return 0;
}

//'@title Function to write the levels of a factor variable to meta data
//'
//'@description Function is intended for internal use
//'
//'@param charName character denoting the meta data name of the factor to be written
//'@param charVec characer denoting the factor levels to be written
//'@param filePath character denoting the location of the h5 file
//'@param update int flag for whether item is new (0) or whether it will overwrite a previous item (1)
//'@return int 0
// [[Rcpp::export]]
int h5WriteFactor(std::string charName, SEXP charVec, std::string filePath, int update)
{
  H5File *file = new H5File(filePath, H5F_ACC_RDWR);
  
  int len = Rf_length(charVec);
  hsize_t DIM1 = len;
  int rank = 1;
  
  // Create a datatype to refer to
  StrType vlst(0, H5T_VARIABLE);
  
  // This is the char array
  char** arr = convertCharArray(charVec);
  
  string meta = "/MetaData/Factor";
  
  // Group Meta Group
  Group* metaGroup = new Group(file->openGroup(meta));
  
  // The dataset and dataspace
  hsize_t dims[] = {DIM1};
  DataSpace dataspace(rank, dims);
  DataSet dataset;
  if(update == 1)
  {
    string slash = "/";
    string groupName = meta + slash + charName;
    file->unlink(groupName);
  }
  dataset = metaGroup->createDataSet(charName, vlst, dataspace);
  dataset.write(arr, vlst);
  dataset.close(); //nn
  metaGroup->close();
  file->close();
  return 0;
}

//'@title Function to write a matrix chunk to file
//'
//'@description Function is intended for internal use
//'
//'@param dset character denoting the meta data name of the data set
//'@param chunk matrix that will be written to h5file
//'@param dim numeric containing the dimension of the matrix that will be written to file
//'@param filePath character denoting the location of the h5 file
//'@return int 0
// [[Rcpp::export]]
int h5WriteDoubleMat (std::string dset, SEXP chunk, NumericVector dim, std::string filePath)
{
  H5File *file = new H5File(filePath, H5F_ACC_RDWR);
  
  // Data initialization.
  int rank = 2;
  hsize_t dims[rank];              // dataset dimensions
  for(int k = 0; k < rank; k++)
    dims[k] = dim(k);
  const void *buf = REAL(chunk);
  
  // Create the data space for the dataset.
  DataSpace dataspace (rank, dims, NULL);
  
  // Create the dataset.
  H5std_string dsetName(dset);
  DataSet dataset = file->createDataSet(dsetName, PredType::NATIVE_DOUBLE, dataspace);

  // Write the data to the dataset using default memory space, file
  // space, and transfer properties.
  dataset.write(buf, PredType::NATIVE_DOUBLE);
  dataset.close(); //nn
  file->close();
  return 0;
}

//'@title Function to finalize h5 file contents allowing them to be read by another user
//'
//'@description The same as the h5CloseFile() function
//'
//'@param filePath character path to the file which will be flushed
//'@return int 0
// [[Rcpp::export]]
int h5FlushFile(std::string filePath)
{
  H5File *file = new H5File(filePath, H5F_ACC_RDWR);
  file->close();
  return 0;
}

//'@title Function to close the h5 file
//'
//'@description Closes the h5 file
//'
//'@param filePath character path to the file which will be flushed
//'@return int 0
// [[Rcpp::export]]
int h5CloseFile(std::string filePath)
{
  H5File *file = new H5File(filePath, H5F_ACC_RDWR);
  file->close();
  return 0;
}

// Reading from file
/*--------------------------------------------------------------------------------------------------*/

CharacterVector ch5ReadCharVector(std::string charName, std::string filePath)
{
  H5File *file = new H5File(filePath, H5F_ACC_RDONLY);
  
  // Group Meta Group
  Group* metaGroup = new Group(file->openGroup("/MetaData"));
  
  // Getting the data set from the file - we cast in place here
  DataSet dataset = metaGroup->openDataSet((H5std_string)charName);
  // Getting the data space from the dataset
  DataSpace dataspace = dataset.getSpace();
  // We know that it is a char vector array so ndim = 1
  hsize_t dims[1];
  // Getting the length of strings
  dataspace.getSimpleExtentDims(dims, NULL);
  
  // for convenience
  int dim = dims[0];
  // String Type
  StrType vlst(0, H5T_VARIABLE);
  // Returning  the data
  char *strRet[dim];
  dataset.read(strRet, vlst);
  // Creating the return data
  CharacterVector out(dim);
  for(int i = 0; i < dim; i++)
  {
    out[i] = strRet[i];
  }
  dataset.close(); //nn
  metaGroup->close();
  file->close();
  return out;
}

//'@title Function to read a character vector from meta data
//'
//'@param charName character the name of the meta data item to be read back from file
//'@param filePath character for the path to the file where the item will be read
//'@return character containing the character vector that has been read from meta data
// [[Rcpp::export]]
SEXP h5ReadCharVector(std::string charName, std::string filePath)
{
  return wrap(ch5ReadCharVector(charName, filePath));
}


CharacterVector ch5ReadFactor(string charName, string filePath)
{
  H5File *file = new H5File(filePath, H5F_ACC_RDONLY);
  
  // Group Meta Group
  Group* metaGroup = new Group(file->openGroup("/MetaData/Factor"));
  
  // Getting the data set from the file 
  DataSet dataset = metaGroup->openDataSet((H5std_string)charName);
  // Getting the data space from the dataset
  DataSpace dataspace = dataset.getSpace();
  // We know that it is a char vector array so ndim = 1
  hsize_t dims[1];
  // Getting the length of strings
  dataspace.getSimpleExtentDims(dims, NULL);
  
  // for convenience
  int dim = dims[0];
  // String Type
  StrType vlst(0, H5T_VARIABLE);
  // Returning  the data
  char *strRet[dim];
  dataset.read(strRet, vlst);
  CharacterVector out(dim);
  for(int i = 0; i < dim; i++)
  {
    out[i] = strRet[i];
  }
  dataset.close(); //nn
  metaGroup->close();
  file->close();
  return out;
}

//'@title Function to read factor levels from meta data for a pgiven factor
//'
//'@param charName character denoting the factor from the meta data factor
//'@param filePath character denoting the path to the h5 file
//'@return character of factor levels
// [[Rcpp::export]]
SEXP h5ReadFactor(std::string charName, std::string filePath)
{
  return wrap(ch5ReadFactor(charName, filePath));
}

//'@title Function to read an integer item from meta data
//'
//'@param intName character for the name of the item to be read back
//'@param filePath character for the path to the h5 file
//'@return int iteger item defined by intName in the meta data
// [[Rcpp::export]]
int h5ReadInt(std::string intName, std::string filePath)
{
  H5File *file = new H5File(filePath, H5F_ACC_RDONLY);
  
  // Group Meta Group
  Group* metaGroup = new Group(file->openGroup("/MetaData"));
  
  // Getting the data set from the file 
  DataSet dataset = metaGroup->openDataSet((H5std_string)intName);
  // Getting the data space from the dataset
  DataSpace dataspace = dataset.getSpace();
  
  // Returning  the data
  int intRet;
  dataset.read(&intRet, PredType::NATIVE_INT);
  
  dataset.close(); //nn
  metaGroup->close();
  file->close();
  return intRet;
}


//'@title Function to read a matrix chunk from a h5 file
//' 
//'@param chunkName the name of the chunk to be read back
//'@param filePath the path to the h5 file
//'@return matrix chunk defined by chunkName
// [[Rcpp::export]]
SEXP h5ReadDoubleMat(std::string chunkName, std::string filePath)
{ 
  // Open the file in Read/Write Mode, H5F_ACC_RDONLY
  H5File *file = new H5File(filePath, H5F_ACC_RDONLY);
  // Opening the data set 
  DataSet dataset = file->openDataSet((H5std_string)chunkName);
  // Opening the data space
  DataSpace dataspace = dataset.getSpace();
  // Get the number of dimensions
  int ndim = dataspace.getSimpleExtentNdims();
  // Create a dimension object to be filled with the dimensions of the data set
  hsize_t dims[ndim];
  // Fill the dimension of the dataset
  dataspace.getSimpleExtentDims(dims, NULL);
  // Create the return data
  SEXP data;
  // Allocating a matrix of the right size and dimension
  data = PROTECT(Rf_allocMatrix(REALSXP, dims[0], dims[1]));
  // Filling the matrix with data form the dataspace
  dataset.read(REAL(data), PredType::NATIVE_DOUBLE, dataspace);
  UNPROTECT(1);
  
  dataset.close(); //nn
  file->close();
  
  return data;
}

//'@title Function for dummy read
//' 
//'@param chunkName the name of the chunk to be read back
//'@param filePath the path to the h5 file
//'@return int 0
// [[Rcpp::export]]
int h5DummyRead(std::string chunkName, std::string filePath)
{ 
  // Open the file in Read/Write Mode, H5F_ACC_RDONLY
  H5File *file = new H5File(filePath, H5F_ACC_RDONLY);
  // Opening the data set 
  DataSet dataset = file->openDataSet((H5std_string)chunkName);
  // Opening the data space
  DataSpace dataspace = dataset.getSpace();
  // Get the number of dimensions
  int ndim = dataspace.getSimpleExtentNdims();
  // Create a dimension object to be filled with the dimensions of the data set
  hsize_t dims[ndim];
  // Fill the dimension of the dataset
  dataspace.getSimpleExtentDims(dims, NULL);
  // Create the return data
  SEXP data;
  // Allocating a matrix of the right size and dimension
  data = PROTECT(Rf_allocMatrix(REALSXP, dims[0], dims[1]));
  // Filling the matrix with data form the dataspace
  dataset.read(REAL(data), PredType::NATIVE_DOUBLE, dataspace);
  UNPROTECT(1);
  
  dataset.close();
  file->close();
  
  return 0;
}



/*--------------------------------------------------------------------------------------------------*/

//' @title Fast model frame for activeReg
//' 
//' @description Function returns a scaled down model frame essentially returning list with no NA values.
//' Each item in the list represents a column in the data frame.
//' 
//' @param chunkName character name of the chunk to be read
//' @param selCols character vector of columns to select
//' @param filePath character path to file where chunk is to be read from
//' @return list representing a data frame with no NA values.
//[[Rcpp::export]]
SEXP h5ModelFrame(std::string chunkName, SEXP selCols_, std::string filePath)
{ 
  // Quick conversion of the SEXP column selection to character vector
  CharacterVector selCols(selCols_);
  // Open the file in Read/Write Mode, H5F_ACC_RDONLY
  H5File *file = new H5File(filePath, H5F_ACC_RDONLY);
  // Opening the data set 
  DataSet dataset = file->openDataSet((H5std_string)chunkName);
  // Opening the data space
  DataSpace dataspace = dataset.getSpace();
  // Get the number of dimensions
  int ndim = dataspace.getSimpleExtentNdims();
  // Create a dimension object to be filled with the dimensions of the data set
  hsize_t dims[ndim];
  // Fill the dimension of the dataset
  dataspace.getSimpleExtentDims(dims, NULL);
  // Create the return data
  // Filling the matrix with data form the dataspace
  SEXP data;
  // Allocating a matrix of the right size and dimension
  data = PROTECT(Rf_allocMatrix(REALSXP, dims[0], dims[1]));
  // Filling the matrix with data form the dataspace
  dataset.read(REAL(data), PredType::NATIVE_DOUBLE, dataspace);
  UNPROTECT(1);
  // Convert the R object to a numeric matrix
  NumericMatrix M__(data);
  CharacterVector colNames = ch5ReadCharVector("ColumnNames", filePath);
  CharacterVector colClasses = ch5ReadCharVector("ColumnClasses", filePath);
  // Create the output
  List DF;
  string colName;
  string colClass;
  NumericVector vect;
  CharacterVector levels;
  int n = selCols.size();
  IntegerVector sel(n);
  int selN;
  NumericMatrix M_(M__.nrow(), n);
  // Find which of the columns has been selected
  sel = match(selCols, colNames);
  // Copy the correct matrix columns
  for(int i = 0; i < n; i++)
  {
    selN = sel[i] - 1;
    M_(_, i) = M__(_, selN);
  }
  // Number of rows in the matrix
  int nr = M_.nrow();
  int goodRow;
  NumericVector goodRows(nr);
  int badRow;
  for(int i = 0; i < nr; i++)
  {
    badRow = sum(is_na(M_(i, _)));
    if(badRow >= 1)
    {
      goodRows[i] = 0;
    }else{
      goodRows[i] = 1;
    }
  }
  //goodRows = goodRows*-1 + 1;
  NumericMatrix M(sum(goodRows), n);
  int j = 0;
  // Remove NA rows
  for(int i = 0; i < nr; i++)
  {
    goodRow = goodRows[i];
    if(goodRow == 1)
    {
      M(j, _) = M_(i, _);
      j++;
    }
  }
  // Compile the list
  for(int i = 0; i < n; i++)
  {
    colName = selCols[i];
    selN = sel[i] - 1;
    colClass = colClasses[selN];
    if(colClass != "factor")
    {
      DF[colName] = M(_, i); 
    }else{
      vect = M(_, i);
      levels = (CharacterVector)ch5ReadFactor(colName, filePath);
      DF[colName] = cCreateFactor(vect, levels);
    }
    
  }
  dataset.close();
  file->close();
  
  return wrap(DF);
}


// 2014-01-29 New Read DF methods
// Function to return data frame chunk as list
List ch5ChunkList(string chunkName, string filePath)
{ 
  // Open the file in Read/Write Mode, H5F_ACC_RDONLY
  H5File *file = new H5File(filePath, H5F_ACC_RDONLY);
  // Opening the data set 
  DataSet dataset = file->openDataSet((H5std_string)chunkName);
  // Opening the data space
  DataSpace dataspace = dataset.getSpace();
  // Get the number of dimensions
  int ndim = dataspace.getSimpleExtentNdims();
  // Create a dimension object to be filled with the dimensions of the data set
  hsize_t dims[ndim];
  // Fill the dimension of the dataset
  dataspace.getSimpleExtentDims(dims, NULL);
  // Create the return data
  // Filling the matrix with data form the dataspace
  SEXP data;
  // Allocating a matrix of the right size and dimension
  data = PROTECT(Rf_allocMatrix(REALSXP, dims[0], dims[1]));
  // Filling the matrix with data form the dataspace
  dataset.read(REAL(data), PredType::NATIVE_DOUBLE, dataspace);
  UNPROTECT(1);
  // converting the R object to a numeric matrix
  NumericMatrix M = as<NumericMatrix>(data);
  CharacterVector colNames = ch5ReadCharVector("ColumnNames", filePath);
  CharacterVector colClasses = ch5ReadCharVector("ColumnClasses", filePath);
  
  // Create the output
  List DF;
  string colName;
  string colClass;
  NumericVector vec;
  CharacterVector levels;
  for(int i = 0; i < dims[1]; i++)
  {
    colName = colNames[i];
    colClass = colClasses[i];
    if(colClass != "factor")
    {
      DF[colName] = M(_, i); 
    }else{
      vec = M(_, i);
      levels = (CharacterVector)ch5ReadFactor(colName, filePath);
      DF[colName] = cCreateFactor(vec, levels);
    }
    
  }
  
  dataset.close();
  file->close();
  
  return DF;
}

//'@title Function returns a data frame chunk as a list object to R where the final cbind will take place 
//'
//'@param chunkName the name of the chunk to be read
//'@param filePath the path to the h5 file
//'@return List representing chunk data frame
// [[Rcpp::export]]
SEXP h5ChunkList(std::string chunkName, std::string filePath)
{
  return wrap(ch5ChunkList(chunkName, filePath));
}


// Function to return a selected part of a data frame as a list
List ch5ChunkSel(string chunkName, CharacterVector selCols, string filePath)
{ 
  // Open the file in Read/Write Mode, H5F_ACC_RDONLY
  H5File *file = new H5File(filePath, H5F_ACC_RDONLY);
  // Opening the data set 
  DataSet dataset = file->openDataSet((H5std_string)chunkName);
  // Opening the data space
  DataSpace dataspace = dataset.getSpace();
  // Get the number of dimensions
  int ndim = dataspace.getSimpleExtentNdims();
  // Create a dimension object to be filled with the dimensions of the data set
  hsize_t dims[ndim];
  // Fill the dimension of the dataset
  dataspace.getSimpleExtentDims(dims, NULL);
  // Create the return data
  // Filling the matrix with data form the dataspace
  SEXP data;
  // Allocating a matrix of the right size and dimension
  data = PROTECT(Rf_allocMatrix(REALSXP, dims[0], dims[1]));
  // Filling the matrix with data form the dataspace
  dataset.read(REAL(data), PredType::NATIVE_DOUBLE, dataspace);
  UNPROTECT(1);
  // converting the R object to a numeric matrix
  NumericMatrix M = as<NumericMatrix>(data);
  CharacterVector colNames = ch5ReadCharVector("ColumnNames", filePath);
  CharacterVector colClasses = ch5ReadCharVector("ColumnClasses", filePath);
  
  // Create the output
  List DF;
  string colName;
  string colClass;
  NumericVector vec;
  CharacterVector levels;
  int n = selCols.size();
  IntegerVector sel(n);
  int selN;
  // First we need to find which of the columns has been selected
  sel = match(selCols, colNames);
  
  for(int i = 0; i < n; i++)
  {
    colName = selCols[i];
    selN = sel[i] - 1;
    colClass = colClasses[selN];
    if(colClass != "factor")
    {
      DF[colName] = M(_, selN); 
    }else{
      vec = M(_, selN);
      levels = (CharacterVector)ch5ReadFactor(colName, filePath);
      DF[colName] = cCreateFactor(vec, levels);
    }
    
  }
  
  dataset.close();
  file->close();
  
  return DF;
}

//'@title Function returns a column subset of data frame chunk as a list
//'
//'@param chunkName the name of the chunk to be read
//'@param selCols the columns that will be selected
//'@param filePath the path to the h5 file
//'@return List representing chunk data frame
// [[Rcpp::export]]
SEXP h5ChunkSel(std::string chunkName, SEXP selCols, std::string filePath)
{
  CharacterVector SelCols = as<CharacterVector>(selCols);
  return wrap(ch5ChunkSel(chunkName, SelCols, filePath));
}


/*--------------------------------------------------------------------------------------------------*/

//'@title Legacy function to return a data frame chunk as a list
//'
//'@description Experimental function not intended for use at all
//'
//'@param chunkName the name of the chunk to be read
//'@param filePath the path to the h5 file
//'@return List of the data frame chunk
// [[Rcpp::export]]
SEXP h5ReadDoubleMat2(std::string chunkName, std::string filePath)
{ 
  // Open the file in Read/Write Mode, H5F_ACC_RDONLY
  H5File *file = new H5File(filePath, H5F_ACC_RDONLY);
  // Opening the data set 
  DataSet dataset = file->openDataSet((H5std_string)chunkName);
  // Opening the data space
  DataSpace dataspace = dataset.getSpace();
  // Get the number of dimensions
  int ndim = dataspace.getSimpleExtentNdims();
  // Create a dimension object to be filled with the dimensions of the data set
  hsize_t dims[ndim];
  // Fill the dimension of the dataset
  dataspace.getSimpleExtentDims(dims, NULL);
  // Create the return data
  // Filling the matrix with data form the dataspace
  SEXP data;
  // Allocating a matrix of the right size and dimension
  data = PROTECT(Rf_allocMatrix(REALSXP, dims[0], dims[1]));
  // Filling the matrix with data form the dataspace
  dataset.read(REAL(data), PredType::NATIVE_DOUBLE, dataspace);
  UNPROTECT(1);
  // converting the R object to a numeric matrix
  NumericMatrix M = as<NumericMatrix>(data);
  List out;
  NumericVector vec(dims[0]);
  CharacterVector colNames = ch5ReadCharVector("ColumnNames", filePath);
  CharacterVector colClasses = ch5ReadCharVector("ColumnClasses", filePath);
  string colName;
  for(int i = 0; i < dims[1]; i++)
  {
    NumericVector vec(dims[0]);
    for(int j = 0; j < dims[0]; j++)
    {
      vec(j) = M(j,i);
    }
    colName = colNames[i];
    if(colClasses[i] == "factor")
    {
      CharacterVector levels;
      levels = ch5ReadFactor(colName, filePath);
      IntegerVector fact(vec.size());
      fact = cCreateFactor(vec, levels);
      out[colName] = fact;
    }else{
      out[colName] = vec;
    }
    
  }
  dataset.close(); //nn
  file->close();
  // Returning the data
  return wrap(out);
}

//'@title Legacy function to return a data frame chunk as a list
//'
//'@description Experimental function not intended for use at all
//'
//'@param chunkName the name of the chunk to be read
//'@param filePath the path to the h5 file
//'@return List of the data frame chunk
// [[Rcpp::export]]
SEXP h5ReadDoubleMat3(std::string chunkName, std::string filePath)
{ 
  // Open the file in Read/Write Mode, H5F_ACC_RDONLY
  H5File *file = new H5File(filePath, H5F_ACC_RDONLY);
  // Opening the data set 
  DataSet dataset = file->openDataSet((H5std_string)chunkName);
  // Opening the data space
  DataSpace dataspace = dataset.getSpace();
  // Get the number of dimensions
  int ndim = dataspace.getSimpleExtentNdims();
  // Create a dimension object to be filled with the dimensions of the data set
  hsize_t dims[ndim];
  // Fill the dimension of the dataset
  dataspace.getSimpleExtentDims(dims, NULL);
  // Create the return data
  // Filling the matrix with data form the dataspace
  //double (*buf)[dims[1]]*[dims[0]] = malloc(dims[1]]*[dims[0] * sizeof *buf);
  //buf[dims[1]][dims[0]] = 0.0;
  double **buf = (double**) calloc (dims[1]*dims[0], sizeof(double));
  buf[dims[1]][dims[0]] = 0.0;
  //double buf[dims[1]][dims[0]];
  dataset.read(buf, PredType::NATIVE_DOUBLE, dataspace);
  // Attempt tp append the contents to a list
  List out;
  NumericVector vec(dims[0]);
  NumericMatrix M(dims[0], dims[1]);
  CharacterVector colNames = ch5ReadCharVector("ColumnNames", filePath);
  CharacterVector colClasses = ch5ReadCharVector("ColumnClasses", filePath);
  string colName;
  for(int i = 0; i < dims[1]; i++)
  {
    NumericVector vec(dims[0]);
    for(int j = 0; j < dims[0]; j++)
    {
      M(j,i) = buf[i][j];
      vec(j) = buf[i][j];
    }
    colName = colNames[i];
    if(colClasses[i] == "factor")
    {
      CharacterVector levels;
      levels = h5ReadFactor(colName, filePath);
      IntegerVector fact(vec.size());
      fact = cCreateFactor(vec, levels);
      out[colName] = fact;
    }else{
      out[colName] = vec;
    }
    
  }
  free(buf);
  
  dataset.close(); //nn
  file->close();
  
  return wrap(out);
}

// Functions for reading and writing data frame and matrix chunks to memory
/*--------------------------------------------------------------------------------------------------*/

//'@title Function to write a matrix to memory and return a pointer
//'
//'@description Intended for internal use only
//'
//'@param obj matrix to be written to memory
//'@return externalptr 
// [[Rcpp::export]]
SEXP h5WriteMemMAT(NumericMatrix obj)
{
  XPtr<NumericMatrix> ptr(new NumericMatrix(obj), true);
  return ptr;
}

//'@title Function to write a data frame object to memory and return a pointer
//'
//'@description Intended for internal use only
//'
//'@param obj data frame to be written to memory
//'@return externalptr 
// [[Rcpp::export]]
SEXP h5WriteMemDF(DataFrame obj)
{
  XPtr<DataFrame> ptr(new DataFrame(obj), true);
  return ptr;
}

//'@title Function to read a matrix from memory when given the external pointer
//'
//'@description Intended for internal use only
//'
//'@param ptr an externalptr to the matrix object to be retrieved
//'@return matrix that is held at the externalptr
// [[Rcpp::export]]
NumericMatrix h5ReadMemMAT(SEXP ptr)
{
  XPtr<NumericMatrix> out(ptr);
  return *out;
}

//'@title Function to read a data frame that has been stored in memory when given the external pointer
//'
//'@param ptr externalptr to data frame stored in memory
//'@return the data frame that is helpd at the externalptr
// [[Rcpp::export]]
DataFrame h5ReadMemDF(SEXP ptr)
{
  XPtr<DataFrame> out(ptr);
  return *out;
}

