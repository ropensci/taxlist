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

# Write data set
source("data-raw/Easplist/Easplist.R")

# Purl vignette R-code
purl("vignettes/taxlist-intro.Rmd", "vignettes/taxlist-intro.R")

# Check application of good practices
gp()

# Build package
Root <- sub("/taxlist", "", getwd(), fixed=TRUE)
Ploc <- build(path=file.path(Root, "00_Rpackages"))

# Test the package
Sys.setenv(LANG="en_US.iso88591")
check_built(path=Ploc)

# After check ------------------------------------------------------------------

# Install the package
## install()

# TODO: Build vignette for homepage

# Render readme-file.
render("README.Rmd")
