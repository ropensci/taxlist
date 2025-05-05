context("Sort taxonomic lists.")

test_that("Sorting is working", {
    expect_is(sort_taxa(Easplist), "taxlist")
    expect_is(sort_taxa(Easplist, priority = c("Cyperus", "Cordia")), "taxlist")
})
