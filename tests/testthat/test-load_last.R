context("loading a backup")

test_that("load_last is working", {
			expect_is({
						load_last(file.path(path.package("taxlist"), "extdata",
										"Podocarpus"))
						Podocarpus
					}, "taxlist")
		}
)
