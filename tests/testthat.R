# browseURL("https://kbroman.org/pkg_primer/pages/tests.html")
# browseURL(file.path("https://walczak.org/2017/06",
# 		"how-to-add-code-coverage-codecov-to-your-r-package/"))

library(testthat)
library(taxlist)

test_check("taxlist")
