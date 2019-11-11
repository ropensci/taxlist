# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("Creating backups")

# Loading installed examples
load("examples.rda")

test_that("Backups of objects are written", {
			expect_output_file(backup_object(data1, file="backup", stamp=FALSE,
							overwrite=TRUE), "backup.rda")
		}
)
