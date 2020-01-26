context("deprecated functions")

test_that("deprecated functions produce messages", {
			expect_warning(add_parent())
			expect_warning(add_trait())
			expect_warning(add_level())
			expect_warning(replace_view())
		}
)
