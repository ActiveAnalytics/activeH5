# Tests for h5MAT objects

context("h5MAT Tests")

test_that("Check creating file and meta data of h5MAT object", {
  testH5 <- "mat.h5"
  set.seed(0)
  mat <- matrix(runif(100*5), nc = 5)
  mt1 <- newH5MAT(mat, testH5, 10)
  expect_true(class(mt1) == "h5MAT", info = "Check that the class is h5MAT")
  expect_true(file.exists(testH5), info = "Test that H5 file was created")
  expect_true(mt1$nrows == 100, "Check that the correct number of rows were written")
  expect_true(mt1$ncols == 5, info = "Check the correct number of cols were written")
  expect_true(all(dim(mt1$readChunk("ch2")) == c(10, 5)), "Read chunk returns the right dimension object")
  expect_true(all(dim(mt1$readChunks(c("ch2", "ch3"))) == c(20, 5)), "Read chunks returns the right dimension object")
  expect_true(identical(mt1$readTable(), mat), info = "Check that correct data is read back")
})

test_that("Check readback of h5 file", {
  testH5 <- "mat.h5"
  mt1 <- openH5MAT(testH5)
  set.seed(0)
  mat <- matrix(runif(100*5), nc = 5)
  expect_true(class(mt1) == "h5MAT", info = "Check that the class is h5MAT")
  expect_true(mt1$nrows == 100, "Check that the correct number of rows were written")
  expect_true(mt1$ncols == 5, info = "Check the correct number of cols were written")
  expect_true(all(dim(mt1$readChunk("ch2")) == c(10, 5)), "Read chunk returns the right dimension object")
  expect_true(identical(mt1$readTable(), mat), info = "Check that correct data is read back")
})

test_that("Check that data is correctly appended to h5 file", {
  testH5 <- "mat.h5"
  mt1 <- openH5MAT(testH5)
  set.seed(0)
  mat1 <- matrix(runif(100*5), nc = 5)
  set.seed(1)
  mat2 <- matrix(runif(50*5), nc = 5)
  mat <- rbind(mat1, mat2)
  mt1$append(mat2)
  mt2 <- mt1$memorize()
  expect_true(class(mt1) == "h5MAT", info = "Check that class of created object is h5MAT")
  expect_true(class(mt2) == "h5MEMMAT", info = "Check that class of memorized object is h5MEMMAT")
  expect_true(mt1$nrows == 150, "Check that the number of rows have been updated")
  expect_true(identical(mt1$readTable(), mat), info = "Check that correct data is read back")
  expect_true(identical(mt1$readTable(), mat), "Check that data was correctly memorized")
  unlink(testH5) # file cleanup
})

