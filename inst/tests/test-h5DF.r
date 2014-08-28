# Tests for h5DF objects

context("h5DF Tests")

test_that("Check creating file and meta data of h5DF object", {
  testH5 <- "iris.h5"
  ir1 <- newH5DF(iris[1:100,], testH5, 10)
  expect_true(class(ir1) == "h5DF", info = "Check that the class is h5DF")
  expect_true(file.exists(testH5), info = "Test that H5 file was created")
  expect_true(ir1$nrows == 100, "Check that the correct number of rows were written")
  expect_true(ir1$ncols == 5, info = "Check the correct number of cols were written")
  expect_true(all(ir1$colClasses == sapply(iris, class)), info = "Check correct column classes written")
  expect_true(ir1$nfactors == 1, "Check the number of factors written")
  expect_true(all(dim(ir1$readChunk("ch2")) == c(10, 5)), "Read chunk returns the right dimension object")
  expect_true(all(dim(ir1$readChunks(c("ch2", "ch3"))) == c(20, 5)), "Read chunks returns the right dimension object")
  expect_true(all(ir1$factors$Species == levels(iris$Species)), "Factor levels check")
  expect_true(identical(ir1$readTable(), iris[1:100,]), info = "Check that correct data is read back")
})

test_that("Check readback of h5 file", {
  testH5 <- "iris.h5"
  ir1 <- openH5DF(testH5)
  expect_true(class(ir1) == "h5DF", info = "Check that the class is h5DF")
  expect_true(ir1$nrows == 100, "Check that the correct number of rows were written")
  expect_true(ir1$ncols == 5, info = "Check the correct number of cols were written")
  expect_true(all(ir1$colClasses == sapply(iris, class)), info = "Check correct column classes written")
  expect_true(ir1$nfactors == 1, "Check the number of factors written")
  expect_true(all(dim(ir1$readChunk("ch2")) == c(10, 5)), "Read chunk returns the right dimension object")
  expect_true(all(ir1$factors$Species == levels(iris$Species)), "Factor levels check")
  expect_true(identical(ir1$readTable(), iris[1:100,]), info = "Check that correct data is read back")
})

test_that("Check that data is correctly appended to h5 file", {
  testH5 <- "iris.h5"
  ir1 <- openH5DF(testH5)
  ir1$append(iris[101:150, ])
  ir2 <- ir1$memorize()
  expect_true(class(ir1) == "h5DF", info = "Check that class of created object is h5DF")
  expect_true(class(ir2) == "h5MEMDF", info = "Check that class of memorized object is h5MEMDF")
  expect_true(ir1$nrows == 150, "Check that the number of rows have been updated")
  expect_true(identical(ir1$readTable(), iris), info = "Check that correct data is read back")
  ir3 <- do.call(rbind, lapply(1:length(ir2$ptrs), ir2$readChunk))
  expect_true(identical(ir3, iris), "Check that data was correctly memorized")
  unlink(testH5) # file cleanup
})


