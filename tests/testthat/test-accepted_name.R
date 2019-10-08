# TODO:   Test function accepted_name
# 
# Author: Miguel Alvarez
################################################################################

context("accepted name manipulation")

test_that("error when using name of different concept as accepted name", {
			expect_error(accepted_name(Easplist, 50074) <- 51129)
		}
)
