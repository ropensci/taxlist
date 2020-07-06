context("cleaning objects")

tmp <- Easplist
tmp@taxonNames <- Easplist@taxonNames[-c(1:10), ]

test_that("disrupt and clean", {
			expect_error(validObject(tmp))
			expect_equal(validObject(clean(tmp)), TRUE)
		}
)
