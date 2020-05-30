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

# Render readme-file.
render("README.Rmd")

# Write data set
source("data-raw/Easplist/Easplist.R")

# Check application of good practices
gp()

# Build package
Root <- sub("/taxlist", "", getwd(), fixed=TRUE)
Ploc <- build(path=file.path(Root, "00_Rpackages"))

# Test the package
Sys.setenv(LANG="en_US.iso88591")
check_built(path=Ploc)
