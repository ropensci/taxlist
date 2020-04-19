context("writing backups")

test_that("Backups are written", {
			backup_object(Easplist, file="temp", stamp=FALSE, overwrite=TRUE)
			expect_true(file.exists("temp.rda"))
			backup_object(Easplist, file="temp", overwrite=TRUE)
			expect_true(file.exists(paste0("temp_", format(Sys.Date(),
											"%Y-%m-%d"),".rda")))
		})
