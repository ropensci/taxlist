# TODO:   Working script for testing the package 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

# Check
library(rmarkdown)
library(devtools)
library(styler)
library(covr)

# Automatic styling
style_pkg()

# document package
## unlink("NAMESPACE")
document()

# clean built package and manual
Folder <- "build-pkg"
Files <- list.files(Folder, ".tar.gz|.pdf")
unlink(file.path(Folder, Files))

# Re-build package and manual
pkg_loc <- build(path = Folder, args = "--resave-data")
## build_manual(path = Folder)

# common check
check_built(path = pkg_loc)

# check coverage
report()

# Special steps ----------------------------------------------------------------
install()
build_manual(path = Folder)

################################################################################

# If error due to vignettes
pkg_loc <- build(path = Folder,
    args = c("--resave-data", "--no-build-vignettes"))
check_built(path = pkg_loc)


################################################################################

library(devtools)
library(styler)
library(knitr)
## library(qpdf)
library(covr)

# Clean session
rm(list = ls())

# Clean folder
unlink(file.path("build-pkg", list.files("build-pkg", ".tar.gz")))
unlink(file.path("build-pkg", list.files("build-pkg", ".pdf")))

# re-style scripts
style_pkg()

# Write data
source("data-raw/create-data.R")

# Purl vignette R-code
purl("vignettes/taxlist-intro.Rmd", "vignettes/taxlist-intro.R")

# write documentation
document()

# Build and check package
Folder = "build-pkg"
## pkg_loc <- build(path = Folder, args = "--resave-data")
pkg_loc <- build(path = Folder,
    args = c("--resave-data", "--no-build-vignettes"))
check_built(path = pkg_loc)

# a posteriori
build_manual(path = Folder)
install()

# Report coverage
report()

# Carry out the tests
test()

# Write data set
## source("data-raw/Easplist/Easplist.R")

# Check application of good practices
gp()

# Codemetar
# write_codemeta()

# Render readme-file.
render("README.Rmd")

# Other checks
library(rhub)

chk <- check_for_cran()
chk$cran_summary()



cran_check_flavours()

cran_check_results("taxlist")

rcmdcheck()




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
