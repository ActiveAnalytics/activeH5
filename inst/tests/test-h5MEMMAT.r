# Tests for h5MEMMAT objects

context("h5MEMMAT Tests")

test_that("Check h5MEMMAT object is correctly created", {
  mat <- matrix(runif(150*5), nc = 5)
  mt1 <- newH5MEMMAT(mat[1:100,], 10)
  expect_true(class(mt1) == "h5MEMMAT", info = "Check that the class is h5MEMMAT")
  expect_true(mt1$nrows == 100, "Check that the correct number of rows were written")
  expect_true(mt1$ncols == 5, info = "Check the correct number of cols were written")
  expect_true(all(dim(mt1$readChunk(2)) == c(10, 5)), "readChunk returns the right dimension object")
  expect_true(all(dim(mt1$readChunks(2:3)) == c(20, 5)), "readChunks returns the right dimension object")
  expect_true(all(dim(mt1$readTable()) == c(100, 5)), "readTable returns the right dimension object")
  mt1$append(mat[101:150, ])
  expect_true(mt1$nrows == 150, "Check that the number of rows have been updated")
  testH5 <- "mat.h5"
  mt2 <- mt1$createH5MAT(testH5)
  expect_true(class(mt2) == "h5MAT", info = "Class check of written h5MAT object")
  expect_true(file.exists(testH5), info = "Test that H5 file was created")
  expect_true(mt2$nrows == 150, "Check that the correct number of rows were written")
  expect_true(mt2$ncols == 5, info = "Check the correct number of cols were written")
  expect_true(all(dim(mt2$readChunk("ch2")) == c(10, 5)), "Read chunk returns the right dimension object")
  expect_true(all(dim(mt2$readChunks(c("ch2", "ch3"))) == c(20, 5)), "Read chunks returns the right dimension object")
  expect_true(identical(mt2$readTable(), mat), info = "Check that correct data is read back")
  unlink(testH5) # cleanup
})
