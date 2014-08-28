# activeH5 is a package for writing/reading large data frames and matrices in chunks

## Introduction

activeH5 provides a set of tools to allow large data frames and matrices to be written to file and to memory and are stored in chunks. On file the data is stored in H5 format and in memory pointers are used to access chunks of data. The data objects are written as reference classes and are h5DF, h5MAT, h5MEMDF, and h5MEMMAT.

With the release of this package open source, it is our intention to more actively maintain and update this. Our first priority is to make the read/write even faster and provide option for compressing the data as well as more user friendly features.

You can visit our website on <http://www.active-analytics.com>.

## Installation

**Pre-requisite**: HDF5 serial version

OS: Linux (Ubuntu), not tested on Windows.

The package can be installed directly from GitHub using the devtools <https://github.com/hadley/devtools> package:

```
require(devtools)
install_github("ActiveAnalytics/activeH5")
require(activeH5)
```

# Quick Intro

Just a quick intro to the basic functions in this package:

```
> ir1 <- newH5DF(iris[1:50,], "iris.h5", 5)
Initializing ...
Creating meta data on file ... 
Converting character to factors 
Registering any factor columns 
Creating the matrix for writing ... 
Writing data to file ... 


> ir1 <- newH5DF(iris[1:100,], "iris.h5", 10)
Initializing ...
Creating meta data on file ... 
Converting character to factors 
Registering any factor columns 
Creating the matrix for writing ... 
Writing data to file ... 


> ir1$append(iris[101:150, ])
Updating meta data on file ... 
Appending data to file ... 

# The names of the chunks
> ir1$chunkNames
[1] "ch1"  "ch2"  "ch3"  "ch4"  "ch5"  "ch6"  "ch7"  "ch8"  "ch9"  "ch10" "ch11" "ch12" "ch13" "ch14" "ch15"

# Size of the chunks
> ir1$chunkSize
[1] 10

# Names of the columns
> ir1$colNames
[1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"

# Reading a chunk
> ir1$readChunk("ch1")
  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
1          5.1         3.5          1.4         0.2  setosa
2          4.9         3.0          1.4         0.2  setosa
3          4.7         3.2          1.3         0.2  setosa
4          4.6         3.1          1.5         0.2  setosa
5          5.0         3.6          1.4         0.2  setosa

> ir1$readTable()
Reading the data from H5 file ... 
H5 data read, now formatting the data ... 
    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
1            5.1         3.5          1.4         0.2     setosa
2            4.9         3.0          1.4         0.2     setosa
3            4.7         3.2          1.3         0.2     setosa
4            4.6         3.1          1.5         0.2     setosa
5            5.0         3.6          1.4         0.2     setosa
6            5.4         3.9          1.7         0.4     setosa
7            4.6         3.4          1.4         0.3     setosa
8            5.0         3.4          1.5         0.2     setosa
9            4.4         2.9          1.4         0.2     setosa
10           4.9         3.1          1.5         0.1     setosa
...

# The meta data
> ir1
Reference class object of class "h5DF"
Field "nChunks":
[1] 15
Field "chunkNames":
 [1] "ch1"  "ch2"  "ch3"  "ch4"  "ch5"  "ch6"  "ch7"  "ch8"  "ch9"  "ch10" "ch11" "ch12" "ch13" "ch14" "ch15"
Field "colNames":
[1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"     
Field "colClasses":
Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
   "numeric"    "numeric"    "numeric"    "numeric"     "factor" 
Field "nrows":
[1] 150
Field "ncols":
[1] 5
Field "filePath":
[1] "iris.h5"
Field "nfactors":
[1] 1
Field "factors":
$Species
[1] "setosa"     "versicolor" "virginica" 

Field "chunkSize":
[1] 10
Field "MAT":
     [,1]
[1,]    0
Field "oldNChunks":
[1] 0
```

For more information:

```
?activeH5
?newH5DF
?openH5DF
?openH5MAT
?newH5MAT
?newH5MEMDF
?newH5MEMMAT
```

All the functions in the package namespace can be viewed using:

```
objects("package:activeH5")
```
... and they all have associated `?help` documentation.

# Details

The h5DF object is used to store very large data frames on disk. The data itself is stores as purely numerical data. Character columns are converted to factors and factors to numeric data, then each chunk is stores as a matrix. This means that the I/O speed is fast. Meta data associated with factors are stored in the file and the h5DF object and are recombined with the daat as it is read back into R.

The h5MAT object is used to store very large matrices on disk in chunks. The data is assumed to be numeric data so I/O speed is fast even faster than h5DF.

The h5MEMDF object is used to store very large data frames in memory as chunks with pointers to each chunk. Data can only be stored where there is sufficient memory to accomodate the object(s). Storing data like this makes it easier move, manipulate and work with very large datasets without the constant replication that occurs when using them in normal R functions. Since the data is stored in memory access time is faster than when the data is stored on disk.

The h5MEMMAT object is used to store very large matrices in memory as chunks. Again there are pointers to each chunk of data and there has to be sufficient memory to accomodate the object(s). Storing data like this makes it easier move, manipulate and work with very large datasets without the constant replication that occurs when using them in normal R functions. Since the data is stored in memory access time is faster than when stored on disk.

The HDF5 files generated by h5DF, h5MAT are not designed to be used with other software that use HDF5 objects. Any data that is created using the activeH5 module may not read back as expected with other HDF5 APIs.


## Contact

For help on the package please contact Chibisi Chima-Okereke: chibisi@activeanalytics.co.uk
