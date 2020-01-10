# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("Testing summary outputs")

test_that("Summaries are produced", {
			result <- evaluate_promise(summary(Easplist), print=TRUE)
			expect_equal(grepl("object", result$output), TRUE)
			result <- evaluate_promise(summary(Easplist, "Cyperus pap"),
					print=TRUE)
			expect_equal(grepl("Cyperus", result$output), TRUE)
		})
