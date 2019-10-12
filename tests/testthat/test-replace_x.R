# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("replacing values in vectors")

test_that("replacing values is working", {
			expect_equal(replace_x(c("a", "B", "c"), "B", "b"), letters[1:3])
		}
)

test_that("replacing by index is working", {
			expect_equal(replace_idx(c("a", "B", "c"), 1:3, 2, "b"),
					letters[1:3])
		}
)

test_that("replacing NAs is working", {
			expect_equal(replace_na(c("a", NA, "c"), 1:3, 1:3, c("A","b","C")),
					letters[1:3])
		}
)
