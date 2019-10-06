
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- Use snippet 'render_markdown' for it -->

# taxlist

<!-- Budges -->

[![DOI](https://zenodo.org/badge/54913161.svg)](https://zenodo.org/badge/latestdoi/54913161)
[![Travis Build
Status](https://travis-ci.org/kamapu/taxlist.svg?branch=master)](https://travis-ci.org/kamapu/taxlist)
[![Codecov test
coverage](https://codecov.io/gh/kamapu/taxlist/branch/master/graph/badge.svg)](https://codecov.io/gh/kamapu/taxlist?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/taxlist)](https://cran.r-project.org/package=taxlist)
[![Rdoc](http://www.rdocumentation.org/badges/version/taxlist)](http://www.rdocumentation.org/packages/taxlist)
[![CRAN\_downloads](http://cranlogs.r-pkg.org/badges/taxlist)](https://cran.r-project.org/package=taxlist)
[![total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/taxlist)](https://cran.r-project.org/package=taxlist)

The aim of `taxlist` is to provide an object structure for taxonomic
lists and methods to display and handle the contained information. We
welcome any interest to implement it or to contribute in the development
of `taxlist`.

Objects and functions of `taxlist` are also implemented as part of
objects containing information from vegetation-plot databases (look at
[vegtable](https://github.com/kamapu/vegtable)). The structure of
`taxlist` objects is strongly inspired on data handled by
[Turboveg](https://www.synbiosys.alterra.nl/turboveg) and on relational
database models.

![](README-figures/taxlist_model.png)<br/> **Figure:** Relational model
for taxlist objects (see [Alvarez & Luebert
2018](https://doi.org/10.3897/BDJ.6.e23635)).

The functions in this package has being used to structure and clean data
stored in [SWEA-Dataveg](http://www.givd.info/ID/AF-00-006), a
vegetation-plot database managed in the context of the project
[GlobE-wetlands](https://www.wetlands-africa.de) and currently exported
to a [PostgreSQL](https://www.postgresql.org) format.

## Installing taxlist

Two alternative options are available for the installation of taxlist.
The first one is installing from the repository at
[CRAN](https://cran.r-project.org/package=taxlist):

``` r
install.packages("taxlist", dependencies=TRUE)
```

The second alternative is to install the developing version from
[GitHub](https://github.com/kamapu/taxlist). For this you will require
the package `devtools`:

``` r
library(devtools)
install_github("kamapu/taxlist", build_vignette=TRUE)
```

## Building taxlist objects

I will take an example from “Helechos de Chile” **(Gunkel 1984)** to
demonstrate how to construct a `taxlist` object from its building
blocks. The first step will be to generate an empty `taxlist` object:

``` r
library(taxlist)
#> This is taxlist 0.1.7
#> 
#> Attaching package: 'taxlist'
#> The following object is masked from 'package:base':
#> 
#>     levels

Fern <- new("taxlist")
summary(Fern)
#> object size: 5.1 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of taxon usage names: 0 
#> number of taxon concepts: 0 
#> trait entries: 0 
#> number of trait variables: 0 
#> taxon views: 0
```

As you can see, there is nothing in there. We start including taxonomic
levels, we like to insert in the list. Remember, the levels have to be
provided in an upward sequence, that is to say from lower to higher
levels:

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
#> object size: 6.1 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of taxon usage names: 4 
#> number of taxon concepts: 4 
#> trait entries: 0 
#> number of trait variables: 0 
#> taxon views: 0 
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

Now set the parent-child relations. Relating to the previous display,
you know that the species (concept ID **1**) is the parent of the
varieties (IDs **2** and **3**), and the genus (ID **4**) is the parent
of the species (ID **1**). Thus the relationships are set as:

``` r
Fern <- update_concept(Fern, ConceptID=c(2,3), Parent=1)
Fern <- update_concept(Fern, ConceptID=1, Parent=4)
summary(Fern)
#> object size: 6.2 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of taxon usage names: 4 
#> number of taxon concepts: 4 
#> trait entries: 0 
#> number of trait variables: 0 
#> taxon views: 0 
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
Fern <- add_synonym(Fern, ConceptID=2, TaxonName="Asplenium sphenoides",
    AuthorName="Kunze")
summary(Fern, "Asplenium sphenoides")
#> ------------------------------ 
#> concept ID: 2 
#> view ID: none 
#> level: variety 
#> parent: 1 Asplenium obliquum Forster 
#> 
#> # accepted name: 
#> 2 Asplenium obliquum var. sphenoides (Kunze) Espinosa 
#> 
#> # synonyms (1): 
#> 5 Asplenium sphenoides Kunze 
#> ------------------------------
```

Hierarchical levels, parent-child relationships and synonyms are
included in the exemplary data `Easplist`. For further functions, look
to the package’s manual.

## Acknowledgements

The author thanks **Stephan Hennekens**, developer of
[Turboveg](http://www.synbiosys.alterra.nl/turboveg), for his patience
and great support finding a common language between `R` and `Turboveg`,
as well as for his advices on formatting `EA-Splist`.

Also thanks to **Federico Luebert** for the fruitful discussions
regarding the terminology used in this project.
