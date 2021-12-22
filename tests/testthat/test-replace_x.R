context("replacing values in vectors")

test_that("replacing values is working", {
  expect_equal(replace_x(c("a", "B", "c"), "B", "b"), letters[1:3])
})

test_that("replacing by index is working", {
  expect_equal(
    replace_idx(c("a", "B", "c"), 1:3, 2, "b"),
    letters[1:3]
  )
})

test_that("replacing NAs is working", {
  expect_equal(
    replace_na(c("a", NA, "c"), 1:3, 1:3, c("A", "b", "C")),
    letters[1:3]
  )
})

test_that("inserting new rows and columns simultaneously", {
  tmp <- iris
  tmp$Species <- paste(iris$Species)
  tmp2 <- data.frame(
    Species = rep("humilis", 2), Height = c(15, 20),
    stringsAsFactors = FALSE
  )
  expect_is(insert_rows(tmp, tmp2), "data.frame")
  expect_equal(nrow(tmp) < nrow(insert_rows(tmp, tmp2)), TRUE)
  expect_equal(ncol(tmp) < ncol(insert_rows(tmp, tmp2)), TRUE)
})
