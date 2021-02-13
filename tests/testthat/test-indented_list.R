context("Testing indented lists")

test_that("Summaries are produced", {
			result <- evaluate_promise(indented_list(Easplist, "papyrus",
							synonyms = TRUE), print=TRUE)
			expect_equal(grepl("Cyperus", result$output), TRUE)
			result <- evaluate_promise(indented_list(Easplist, "Cyperus",
							indent = c(family = "fam. ", genus = " gen. ",
									species = "  spec. ",
									subspecies = "   subspec. ",
									variety = "    var. ")), print=TRUE)
			expect_equal(grepl("Cyperus", result$output), TRUE)
		})
