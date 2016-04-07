<!-- README.md is generated from README.Rmd. Please edit that file -->



# taxlist

The aim of `taxlist` is to provide an object structure for taxonomic lists
and a set some methods to handle the contained information.
In a first attempt (let us call it the **_first weave_**) this package will
handle the flat format used by
[Turboveg](http://www.synbiosys.alterra.nl/turboveg).
In a second attempt (the **_second weave_**, of course) we may try to insert a
hierarchical structure to those objects.

This package has been developed as a tool handling data stored in
[SWEA-Dataveg](http://www.givd.info/ID/AF-00-006).
This activity is running in the context of the project
[GlobE-wetlands](https://www.wetlands-africa.de/).

## Updating to the last version of taxlist
You may have previously installed the R-package
[devtools](https://github.com/hadley/devtools).
Then execute following commands in your R-session:


```r
library(devtools)
install_github("kamapu/taxlist")
```

## Some examples

### Working with East African plants (on-going project)

Our current vegetation database is connected to the species list **EA-Splist**.
This list is uses as main reference the
[African Plant Database](http://www.ville-ge.ch/musinfo/bd/cjb/africa/recherche.php)
for nomenclature.
You can use following code to load the last version of the species list:


```r
library(taxlist)
load(url("https://www.wetlands-africa.de/publications/Easplist.rda"))

## Code will be complemented
```
