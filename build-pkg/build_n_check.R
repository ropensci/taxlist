# TODO:   Working script for testing the package 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

# See at
## browseURL(paste0("https://www.marinedatascience.co/blog/2020/01/09/",
##                 "checklist-for-r-package-re-submissions-on-cran/"))

# Needed packages
library(devtools)
library(covr)
library(goodpractice)
library(rmarkdown)
library(knitr)
library(pkgdown)
library(codemetar)

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

# Codemetar
# write_codemeta()

# Build package
library(devtools)
document()
pkg_loc <- build(path="build-pkg")

# Test the package
## Sys.setenv(LANG="en_US.iso88591")
## Sys.setlocale("LC_ALL", "en_US.iso88591")
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
check_built(path=pkg_loc)

# After check ------------------------------------------------------------------

# Install the package
## install()

# Render readme-file.
render("README.Rmd")

# Check on Win-builder
browseURL("https://win-builder.r-project.org/")

# submit to CRAN


# Render package-site
# pkgdown::build_home()
# Render package-site
## usethis::use_pkgdown()
## pkgdown::build_site(preview=FALSE)
## 
## # Copy site
## r_path <- gsub("/taxlist", "", getwd())
## pkg_path <- file.path(r_path, "kamapu.github.io", "rpkg")
## 
## file.copy("docs", pkg_path, recursive=TRUE)
## unlink("docs", recursive=TRUE)
## 
## unlink(file.path(pkg_path, "taxlist"), recursive=TRUE)
## file.rename(file.path(pkg_path, "docs"), file.path(pkg_path, "taxlist"))
## 
## file.copy("README-figures", file.path(pkg_path, "taxlist"), recursive=TRUE)
