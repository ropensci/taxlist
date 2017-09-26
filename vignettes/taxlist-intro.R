## ----install_github, eval=FALSE------------------------------------------
#  library(devtools)
#  install_github("kamapu/taxlist", build_vignettes=TRUE)

## ----install_cran, eval=FALSE--------------------------------------------
#  install.packages("taxlist", dependencies=TRUE)

## ----load_taxlist, message=FALSE-----------------------------------------
library(taxlist)

## ----call_vignette, eval=FALSE-------------------------------------------
#  vignette("taxlist-intro")

## ----load_example_table--------------------------------------------------
load(url("https://github.com/kamapu/thisdataismine/raw/master/data/Cross.rda"))

## ----head_example--------------------------------------------------------
head(Cross[,1:8])

## ----character2taxlist---------------------------------------------------
Splist <- Cross[,"TaxonName"]
Splist <- df2taxlist(Splist)
summary(Splist)

## ----summary_character---------------------------------------------------
summary(Splist, "Erigeron floribundus")

