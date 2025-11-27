# TODO:   Working script for testing the package 'taxlist'
#
# Author: Miguel Alvarez
################################################################################

# Check
library(rmarkdown)
library(devtools)
library(styler)
library(covr)
library(quarto)

# Automatic styling
style_pkg()

# document package
## unlink("NAMESPACE")
document()

# clean built package
unlink(file.path("build-pkg", list.files("build-pkg", ".tar.gz")))

# Build and check package
pkg_loc <- build(path = "build-pkg", args = "--resave-data")
# pkg_loc <- build(path = "build-pkg", args = "--resave-data", vignettes = FALSE)
check_built(path = pkg_loc)

# check coverage
report()

# render README ----------------------------------------------------------------
install()
quarto_render("README.qmd")

# Further steps ----------------------------------------------------------------

# write manual
unlink(file.path("build-pkg", list.files("build-pkg", ".pdf")))
build_manual(path = "build-pkg")
