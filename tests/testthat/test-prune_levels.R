context("Prune superfluous taxonomic ranks.")

test_that("Superfluous ranks get pruned.", {
      Cyperus <- subset(Easplist, TaxonName == "Cyperus", slot = "taxonNames") 
      expect_true(length(levels(Cyperus)) >=
              length(levels(prune_levels(Cyperus))))
    })
