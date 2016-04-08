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

# Test for the validity of the object
validObject(Easplist)
#> [1] TRUE

# A summary of the species list
summary(Easplist)
#> 3924 names for 2580 taxa 
#> 0 (0%) taxa with first name entries
#> 2 variables for taxon traits 
#> validation for class 'taxlist': TRUE

# You can also get a listing of stored names for a species
summary(subset(Easplist, TaxonName == "Cyclosorus interruptus"), "all")
#> ------------------------------ 
#> # Valid name for taxon concept '50074':
#> 50074 Cyclosorus interruptus (Willd.) H. Itô 
#> 
#> # First name: 
#> none 
#> 
#> # Synonyms: 
#> 52002 Dryopteris gongylodes (Schkuhr) Kuntze 
#> 52008 Thelypteris interrupta (Willd.) K. Iwats. 
#> 52009 Cyclosorus striatus Ching 
#> 53097 Pteris interrupta Willd. 
#> 53098 Aspidium continuum Desv. 
#> 53099 Aspidium ecklonii Kunze 
#> 53100 Aspidium gongylodes Schkuhr 
#> 53101 Aspidium obtusatum Sw. 
#> 53102 Aspidium pteroides (Retz.) Sw. 
#> 53103 Aspidium serra (Sw.) Sw. 
#> 53104 Aspidium serratum Sw. 
#> 53105 Aspidium unitum (L.) Sw. 
#> 53106 Nephrodium propinquum R. Br. 
#> ------------------------------
```

## Acknowledgements
The author thanks **Stephan Hennekens**, the developer of
[Turboveg](http://www.synbiosys.alterra.nl/turboveg), for his patience and huge
support finding a common language between **R** and **Turboveg** and for his
advices in the formatting of **EA-Splist**.

Also thanks to **Federico Luebert** for the fruitful discussions regarding the
terminology used in this project.
