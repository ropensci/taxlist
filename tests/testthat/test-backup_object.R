context("writing backups")

test_that("Backups are written", {
  back_log <- backup_object(Easplist, file = file.path(tempdir(), "temp"),
      stamp = FALSE, overwrite = TRUE)
  expect_true(file.exists(back_log["abspath"]))
  back_log <- backup_object(Easplist, file = "temp", overwrite = TRUE)
  expect_true(file.exists(back_log["abspath"]))
  back_log <- backup_object(Easplist, file = "temp")
  expect_true(file.exists(back_log["abspath"]))
  expect_error(sort_backups(name = "apple", path = tempdir()))
})
