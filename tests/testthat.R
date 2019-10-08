# TODO:   Script for test suit
# 
# Author: Miguel Alvarez
################################################################################

# browseURL("https://kbroman.org/pkg_primer/pages/tests.html")
# browseURL("https://walczak.org/2017/06/how-to-add-code-coverage-codecov-to-your-r-package/")

library(testthat)
library(taxlist)

test_check("taxlist")
