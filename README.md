<!-- README.md is generated from README.Rmd. Please edit that file -->



# taxlist

The aim of `taxlist` is to provide an object structure for taxonomic lists
and methods to display and handle the contained information.
This package should be considered as experimental but we welcome any interest
to implement it or contribute to `taxlist`.

**Task One:** The first task is to develop an object class (`taxlist`) for the
import of taxonomic lists structured as _single database list_.
In this step we will pay special attention to the format used by
[Turboveg](http://www.synbiosys.alterra.nl/turboveg) databases.

**Task Two:** While the first task is currently in an advanced stage, the next
step will be the design of an object class implementing hierarchical structure
but accessed in a similar way as for the class `taxlist`.

Objects and functions of `taxlist` will be also implemented as part of objects
containing information from vegetation-plot databases (look at
[vegtables](https://github.com/kamapu/vegtables)).

This package has been developed as a tool handling data stored in
[SWEA-Dataveg](http://www.givd.info/ID/AF-00-006), which is managed in the
context of the project [GlobE-wetlands](https://www.wetlands-africa.de).

## Updating to the last version of taxlist
You may have previously installed the R-package
[devtools](https://github.com/hadley/devtools) (we recommend to use the command
`install.packages("devtools", dependencies=TRUE)`).
After that, you may be able to get the last uploaded version using following
command lines in your `R-Console`:


```r
library(devtools)
install_github("kamapu/taxlist")
```

## Some examples

### Working with East African plants (on-going project)
The vegetation-plot database [SWEA-Dataveg](http://www.givd.info/ID/AF-00-006)
is connected to the species list `EA-Splist`.
This list uses as main nomenclatorial reference the
[African Plant Database](http://www.ville-ge.ch/musinfo/bd/cjb/africa/recherche.php).
An actualized version of the list may be available in the homepage of the
[GlobE-wetlands](https://www.wetlands-africa.de) project.
You may be aware that `EA-Splist` is mainly including names and taxa occurring
in `SWEA-Dataveg` and is not a complete list of the flora for East Africa.


```r
# Load EA-Splist and test the validity of the taxlist-object
library(taxlist)
data(Easplist)
validObject(Easplist)
#> [1] TRUE

# A summary of the species list
summary(Easplist)
#> 4125 names for 2723 taxa 
#> 2 variables for taxon traits 
#> 1 taxon view(s) 
#> validation for class 'taxlist': TRUE

# To know, how many genera and how many families are included in the list
length(unique(Easplist$GENUS))
#> [1] 1026
length(unique(Easplist$FAMILY))
#> [1] 190

# You can also get subsets and listings of all stored names for a species
Cyclosorus <- subset(Easplist, TaxonName == "Cyclosorus interruptus")
summary(Cyclosorus)
#> 14 names for 1 taxon 
#> 2 variables for taxon traits 
#> 1 taxon view(s) 
#> validation for class 'taxlist': TRUE
summary(Cyclosorus, "all")
#> ------------------------------ 
#> # Accepted name for taxon concept '50074' (concept view 1):
#> 50074 Cyclosorus interruptus (Willd.) H. Itô 
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
The author thanks **Stephan Hennekens**, developer of
[Turboveg](http://www.synbiosys.alterra.nl/turboveg), for his patience and great
support finding a common language between `R` and `Turboveg`, as well as for
his advices on formatting `EA-Splist`.

Also thanks to **Federico Luebert** for the fruitful discussions regarding the
terminology used in this project.
