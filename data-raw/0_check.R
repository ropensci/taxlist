# TODO:   Working script for testing the package 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

# Needed packages
library(devtools)
library(covr)
library(goodpractice)
library(rmarkdown)
library(knitr)

# Document package
document()

# Report coverage
report()

# Carry out the tests
test()

# Write data set
source("data-raw/Easplist/Easplist.R")

# Purl vignette R-code
purl("vignettes/taxlist-intro.Rmd", "vignettes/taxlist-intro.R")

# Check application of good practices
gp()

# Build package
pkg_loc <- build(path="built-pkg")

# Test the package
Sys.setenv(LANG="en_US.iso88591")
check_built(path=pkg_loc)

# After check ------------------------------------------------------------------

# Install the package
## install()

# TODO: Build vignette for homepage

# Render readme-file.
render("README.Rmd")
