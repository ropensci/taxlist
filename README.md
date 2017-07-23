
<!-- README.md is generated from README.Rmd. Please edit that file -->
taxlist
=======

<!-- Budges -->
[![Travis Build Status](https://travis-ci.org/kamapu/taxlist.svg?branch=master)](https://travis-ci.org/kamapu/taxlist) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/taxlist)](https://cran.r-project.org/package=taxlist) [![CRAN\_downloads](http://cranlogs.r-pkg.org/badges/taxlist)](http://cran.rstudio.com/web/packages/taxlist/index.html)

The aim of `taxlist` is to provide an object structure for taxonomic lists and methods to display and handle the contained information. This package should be considered as experimental but we welcome any interest to implement it or contribute to `taxlist`.

**Task One:** The first task is to develop an object class (`taxlist`) for the import of taxonomic lists structured as *single database list*. In this step we will pay special attention to the format used by [Turboveg](http://www.synbiosys.alterra.nl/turboveg) databases.

**Task Two:** While the first task is currently in an advanced stage, the next step will be the design of an object class implementing hierarchical structure but accessed in a similar way as for the class `taxlist`.

Objects and functions of `taxlist` will be also implemented as part of objects containing information from vegetation-plot databases (look at [vegtables](https://github.com/kamapu/vegtable)).

This package has been developed as a tool handling data stored in [SWEA-Dataveg](http://www.givd.info/ID/AF-00-006), which is managed in the context of the project [GlobE-wetlands](https://www.wetlands-africa.de).

Installing taxlist
------------------

Two alternative options are available for the installation of taxlist. The first one is installing from the repository at the **Comprehensive R Archive Network**:

``` r
install.packages("taxlist", dependencies=TRUE)
```

Since `taxlist` is developed on **GitHub**, it is also possible to install last version from this repository using the package `devtools`:

``` r
library(devtools)
install_github("kamapu/taxlist", build_vignettes=TRUE)
```

Note that the versions in those repositories may not match completely.

Some examples
-------------

### Working with East African plants (on-going project)

The vegetation-plot database [SWEA-Dataveg](http://www.givd.info/ID/AF-00-006) is connected to the species list `EA-Splist`. This list uses as main nomenclatorial reference the [African Plant Database](http://www.ville-ge.ch/musinfo/bd/cjb/africa/recherche.php). An actualized version of the list may be available in the homepage of the [GlobE-wetlands](https://www.wetlands-africa.de) project. You may be aware that `EA-Splist` is mainly including names and taxa occurring in `SWEA-Dataveg` and is not a complete list of the flora for East Africa.

### Starting with building blocks

I will take an example from "Helechos de Chile" **(Gunkel 1984)** to demonstrate how to construct a `taxlist` object from its building blocks. The first step will be to generate an empty `taxlist` object:

``` r
library(taxlist)

Fern <- new("taxlist")
summary(Fern)
#> object size: 4.9 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of names: 0 
#> number of concepts: 0 
#> trait entries: 0 
#> reference entries: 0
```

As you can see, there is nothing in there. We start including taxonomic levels, we like to insert in the list. Remember, the levels have to be provided in an upward sequence, that is to say from lower to higher levels:

``` r
levels(Fern) <- c("variety","species","genus")
```

Then you can add a species:

``` r
Fern <- add_concept(Fern, TaxonName="Asplenium obliquum", AuthorName="Forster",
    Level="species")
```

Then add varieties:

``` r
Fern <- add_concept(Fern,
    TaxonName=c("Asplenium obliquum var. sphenoides",
        "Asplenium obliquum var. chondrophyllum"),
    AuthorName=c("(Kunze) Espinosa",
        "(Bertero apud Colla) C. Christense & C. Skottsberg"),
    Level="variety")
```

Finally add the genus and check the object:

``` r
Fern <- add_concept(Fern, TaxonName="Asplenium", AuthorName="L.", Level="genus")
summary(Fern)
#> object size: 5.8 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of names: 4 
#> number of concepts: 4 
#> trait entries: 0 
#> reference entries: 0 
#> 
#> hierarchical levels: variety < species < genus 
#> number of concepts in level variety: 2
#> number of concepts in level species: 1
#> number of concepts in level genus: 1

summary(Fern, "all")
#> ------------------------------ 
#> concept ID: 1 
#> view ID: none 
#> level: species 
#> parent: none 
#> 
#> # accepted name: 
#> 1 Asplenium obliquum Forster 
#> ------------------------------ 
#> concept ID: 2 
#> view ID: none 
#> level: variety 
#> parent: none 
#> 
#> # accepted name: 
#> 2 Asplenium obliquum var. sphenoides (Kunze) Espinosa 
#> ------------------------------ 
#> concept ID: 3 
#> view ID: none 
#> level: variety 
#> parent: none 
#> 
#> # accepted name: 
#> 3 Asplenium obliquum var. chondrophyllum (Bertero apud Colla) C. Christense & C. Skottsberg 
#> ------------------------------ 
#> concept ID: 4 
#> view ID: none 
#> level: genus 
#> parent: none 
#> 
#> # accepted name: 
#> 4 Asplenium L. 
#> ------------------------------
```

### Set parent-child relationships and synonyms

Now set the parent-child relations. Relating to the previous display, you know that the species (concept ID **1**) is the parent of the varieties (IDs **2** and **3**), and the genus (ID **4**) is the parent of the species (ID **1**). Thus the relationships are set as:

``` r
add_parent(Fern, c(2,3)) <- 1
add_parent(Fern, 1) <- 4
summary(Fern)
#> object size: 5.8 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of names: 4 
#> number of concepts: 4 
#> trait entries: 0 
#> reference entries: 0 
#> 
#> concepts with parents: 3 
#> concepts with children: 2 
#> 
#> hierarchical levels: variety < species < genus 
#> number of concepts in level variety: 2
#> number of concepts in level species: 1
#> number of concepts in level genus: 1
```

Similarly to the addition of concepts, you can also add synonyms:

``` r
Fern <- add_synonym(Fern, ConceptID=2, TaxonName=c("Asplenium sphenoides"),
    AuthorName="Kunze")
summary(Fern, "all")
#> ------------------------------ 
#> concept ID: 1 
#> view ID: none 
#> level: species 
#> parent: 4 
#> 
#> # accepted name: 
#> 1 Asplenium obliquum Forster 
#> ------------------------------ 
#> concept ID: 2 
#> view ID: none 
#> level: variety 
#> parent: 1 
#> 
#> # accepted name: 
#> 2 Asplenium obliquum var. sphenoides (Kunze) Espinosa 
#> 
#> # synonyms (1): 
#> 5 Asplenium sphenoides Kunze 
#> ------------------------------ 
#> concept ID: 3 
#> view ID: none 
#> level: variety 
#> parent: 1 
#> 
#> # accepted name: 
#> 3 Asplenium obliquum var. chondrophyllum (Bertero apud Colla) C. Christense & C. Skottsberg 
#> ------------------------------ 
#> concept ID: 4 
#> view ID: none 
#> level: genus 
#> parent: none 
#> 
#> # accepted name: 
#> 4 Asplenium L. 
#> ------------------------------
```

Hierarchical levels, parent-child relationships and synonyms are included in the exemplary data `Easplist`. For further functions, look to the package's manual.

Acknowledgements
----------------

The author thanks **Stephan Hennekens**, developer of [Turboveg](http://www.synbiosys.alterra.nl/turboveg), for his patience and great support finding a common language between `R` and `Turboveg`, as well as for his advices on formatting `EA-Splist`.

Also thanks to **Federico Luebert** for the fruitful discussions regarding the terminology used in this project.
