context("Gernerat and solve IDs")

test_that("id_generator() is working", {
  expect_is(id_generator(len = 10, minvalue = 5), "integer")
  expect_is(id_generator(len = 10, mode = "character"), "character")
})

test_that("id_solver() is working", {
  expect_is(id_solver(insert = c(3, 7, 5, 10), to = c(1:5)), "numeric")
  db_refs <- c("Alvarez2003", "Schmitz1988", "Li2023")
  new_refs <- c("Alvarez2003", "Li2023", "Mueller1953", "Alvarez2003a")
  expect_is(id_solver(
    insert = new_refs, to = db_refs,
    suffix = "character"
  ), "character")
  expect_is(id_solver(
    insert = new_refs, to = db_refs,
    suffix = "numeric"
  ), "character")
  expect_error(id_solver(insert = c(3, 7, 7, 5, 10), to = c(1:5)))
  expect_error(id_solver(insert = c(3, 7, 5, 10), to = c(1:5, 5)))
  expect_error(id_solver(insert = c(3, 7, 5, 10), to = c("bla", "ble")))
  expect_error(id_solver(insert = c("bla", "ble"), to = c(3, 7, 5, 10)))
  expect_error(id_generator(len = 10, minvalue = 5, mode = "infinite"))
  expect_error(id_solver(insert = new_refs, to = db_refs, suffix = "infinite"))
})
