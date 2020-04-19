# TODO:   Working script for testing the package 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

# Needed packages
library(devtools)
library(covr)
library(goodpractice)
library(rmarkdown)

# Document package
document()

# Report coverage
report()

# Check application of good practices
gp()

# Render readme-file.
render("README.Rmd")

# Build package
Root <- sub("/taxlist", "", getwd(), fixed=TRUE)
Ploc <- build(path=file.path(Root, "00_Rpackages"))

# Test the package
check_built(path=Ploc)
