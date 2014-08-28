# Tests for h5MEMDF objects

context("h5MEMDF Tests")

test_that("Check h5MEMDF object is correctly created", {
  ir1 <- newH5MEMDF(iris[1:100,], 10)
  expect_true(class(ir1) == "h5MEMDF", info = "Check that the class is h5MEMDF")
  expect_true(ir1$nrows == 100, "Check that the correct number of rows were written")
  expect_true(ir1$ncols == 5, info = "Check the correct number of cols were written")
  expect_true(all(ir1$colClasses == sapply(iris, class)), info = "Check correct column classes written")
  expect_true(ir1$nfactors == 1, "Check the number of factors written")
  expect_true(all(dim(ir1$readChunk(2)) == c(10, 5)), "Read chunk returns the right dimension object")
  expect_true(all(ir1$factors$Species == levels(iris$Species)), "Factor levels check")
  ir1$append(iris[101:150, ])
  expect_true(ir1$nrows == 150, "Check that the number of rows have been updated")
  testH5 <- "iris.h5"
  ir2 <- ir1$createH5DF(testH5)
  expect_true(class(ir2) == "h5DF", info = "Class check of written h5DF object")
  expect_true(file.exists(testH5), info = "Test that H5 file was created")
  expect_true(ir2$nrows == 150, "Check that the correct number of rows were written")
  expect_true(ir2$ncols == 5, info = "Check the correct number of cols were written")
  expect_true(all(ir2$colClasses == sapply(iris, class)), info = "Check correct column classes written")
  expect_true(ir2$nfactors == 1, "Check the number of factors written")
  expect_true(all(dim(ir2$readChunk("ch2")) == c(10, 5)), "Read chunk returns the right dimension object")
  expect_true(all(dim(ir2$readChunks(c("ch2", "ch3"))) == c(20, 5)), "Read chunks returns the right dimension object")
  expect_true(all(ir2$factors$Species == levels(iris$Species)), "Factor levels check")
  expect_true(identical(ir2$readTable(), iris), info = "Check that correct data is read back")
  unlink(testH5) # cleanup
})
