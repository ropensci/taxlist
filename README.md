
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- Use snippet 'render_markdown' for it -->

# taxlist

<!-- Budges -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/taxlist)](https://cran.r-project.org/package=taxlist)
[![Rdoc](http://www.rdocumentation.org/badges/version/taxlist)](http://www.rdocumentation.org/packages/taxlist)
[![DOI](https://zenodo.org/badge/54913161.svg)](https://zenodo.org/badge/latestdoi/54913161)
<br> [![Travis Build
Status](https://travis-ci.org/kamapu/taxlist.svg?branch=master)](https://travis-ci.org/kamapu/taxlist)
[![codecov](https://codecov.io/gh/kamapu/taxlist/branch/master/graph/badge.svg)](https://codecov.io/gh/kamapu/taxlist)
<br>
[![CRAN\_downloads](http://cranlogs.r-pkg.org/badges/taxlist)](https://cran.r-project.org/package=taxlist)
[![total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/taxlist)](https://cran.r-project.org/package=taxlist)

The aim of `taxlist` is to provide an object structure for taxonomic
lists, as well as functions for handling and displaying the information
contained in such objects. The structure of `taxlist` objects is
inspired on data handled by
[Turboveg](https://www.synbiosys.alterra.nl/turboveg) and on relational
database models.

![](README-figures/taxlist_model.png)<br/> **Figure:** Relational model
for `taxlist` objects (see [Alvarez & Luebert
2018](https://doi.org/10.3897/BDJ.6.e23635)).

The functions in this package has being used to structure and clean data
stored in [SWEA-Dataveg](https://kamapu.github.io/sweadataveg.html), a
vegetation-plot database for Eastern Africa.

## Installing taxlist

This package is available from the Comprehensive R Archive Network
(**CRAN**) and can be directly installed in an R-session:

``` r
install.packages("taxlist", dependencies=TRUE)
```

Alternatively, the current development version is available from
[GitHub](https://github.com/kamapu/taxlist) and can be installed using
the package `devtools`:

``` r
library(devtools)
install_github("kamapu/taxlist", build_vignette=TRUE)
```

## Building taxlist Objects

Objects can be built step-by-step as in this example taking as reference
the “Ferns of Chile” (original in Spanish: “Helechos de Chile”) by
**Gunkel (1984)**. The first step will be to generate an empty `taxlist`
object:

``` r
library(taxlist)

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

We start defining the required taxonomic ranks. In such case, the levels
have to be provided from the lowest to highest hierarchical level:

``` r
levels(Fern) <- c("variety","species","genus")
```

For convenience, we start inserting taxa with their respective names in
a top-down direction. We use then the function `add_concept()` to add a
new taxon. Note that the arguments `TaxonName`, `AuthorName`, and
`Level` are used to provide the name of the taxon, the authority of the
name and the taxonomic rank, respectively.

``` r
Fern <- add_concept(Fern, TaxonName="Asplenium", AuthorName="L.", Level="genus")
summary(Fern, "all")
#> ------------------------------ 
#> concept ID: 1 
#> view ID: none 
#> level: genus 
#> parent: none 
#> 
#> # accepted name: 
#> 1 Asplenium L. 
#> ------------------------------
```

As you see, the inserted genus got the concept ID **1** (see
`TaxonConceptID` in the previous figure). To insert a species of this
genus, we use again the function `add_concept()`, but this time we will
also provide the ID of the parent taxon with the argument `Parent`.

``` r
Fern <- add_concept(Fern, TaxonName="Asplenium obliquum", AuthorName="Forster",
    Level="species", Parent=1)
summary(Fern, "Asplenium obliquum")
#> ------------------------------ 
#> concept ID: 2 
#> view ID: none 
#> level: species 
#> parent: 1 Asplenium L. 
#> 
#> # accepted name: 
#> 2 Asplenium obliquum Forster 
#> ------------------------------
```

In the same way, we can add now two varieties of the inserted species:

``` r
Fern <- add_concept(Fern,
    TaxonName=c("Asplenium obliquum var. sphenoides",
        "Asplenium obliquum var. chondrophyllum"),
    AuthorName=c("(Kunze) Espinosa",
        "(Bertero apud Colla) C. Christense & C. Skottsberg"),
    Level="variety", Parent=c(2,2))
```

You may have realized that the function `summary()` is applied to
provide on the one side a display of meta-information for the `taxlist`
object, and on the other side to show a detail of the taxa included in
the object. In the later case adding the keyword `"all"` in the second
argument will show all the handled taxa.

``` r
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

summary(Fern, "all")
#> ------------------------------ 
#> concept ID: 1 
#> view ID: none 
#> level: genus 
#> parent: none 
#> 
#> # accepted name: 
#> 1 Asplenium L. 
#> ------------------------------ 
#> concept ID: 2 
#> view ID: none 
#> level: species 
#> parent: 1 Asplenium L. 
#> 
#> # accepted name: 
#> 2 Asplenium obliquum Forster 
#> ------------------------------ 
#> concept ID: 3 
#> view ID: none 
#> level: variety 
#> parent: 2 Asplenium obliquum Forster 
#> 
#> # accepted name: 
#> 3 Asplenium obliquum var. sphenoides (Kunze) Espinosa 
#> ------------------------------ 
#> concept ID: 4 
#> view ID: none 
#> level: variety 
#> parent: 2 Asplenium obliquum Forster 
#> 
#> # accepted name: 
#> 4 Asplenium obliquum var. chondrophyllum (Bertero apud Colla) C. Christense & C. Skottsberg 
#> ------------------------------
```

## Similar Packages

The package `taxlist` shares similar objectives than the package
[`taxa`](https://github.com/ropensci/taxa), but sing different
approaches for object oriented programming in **R**, namely `taxlist`
uses S4 while `taxa` uses R6. Additionally, `taxa` is rather
developer-oriented, while `taxlist` is rather user-oriented.

In following cases you may prefer to use `taxlist`:

  - In the case that you like to handle taxonomic lists including
    parent-child relationships and synonymy, especially when you need an
    automatic check on the consistency of this information (i.e. using
    the function `validObject()`) or when the information have to be
    re-arranged during a session.
  - When you foresee statistical assessments on taxonomy diversity or
    taxon properties (chorology, conservation status, functional traits,
    etc.).
  - When you seek to produce documents using **rmarkdown**, for instance
    guide books or check-lists. Also in article manuscripts taxonomic
    names referring to a taxon concept can be easily formatted by the
    function `print_name()`.
  - When importing taxonomic lists from databases stored in
    [**Turboveg 2**](http://www.synbiosys.alterra.nl/turboveg).
  - When you seek to implement the package
    [`vegtable`](https://CRAN.R-project.org/package=vegtable) on
    handling and assessing biodiversity records, especially
    vegetation-plot data. In that case, taxonomic lists will be
    formatted as a `taxlist` object.

## Rmarkdown Integration

As mentioned before, `taxlist` objects can be also used for writing
rmarkdown documents (see [this
poster](https://dx.doi.org/10.13140/RG.2.2.36713.90728)). For instance
you can insert your objects at the beginning of the document with a
hidden chunk:

```` markdown
```{r  echo=FALSE, message=FALSE, warning=FALSE}
library(taxlist)
data(Easplist)
```
````

To mention a taxon, you can write in-line codes, such as <code>\`r
print\_name(Easplist, 206)\`</code>, which will insert *Cyperus papyrus*
L. in your document (note that the number is the ID of the taxon concept
in `Easplist`). Fort a second mention of the same species, you can then
use <code>\`r print\_name(Easplist, 206, second\_mention=TRUE)\`</code>,
which will insert *C. papyrus* in your text.

## Descriptive Statistics

Information located in the slot **taxonTraits** are suitable for
statistical assessments. For instance, in the installed object
`Easplist` a column called **lf\_behn\_2018** includes a classification
of macrophytes into different life forms. To know the frequency of these
life forms in the `Easplist`, we can use the function `count_taxa()`:

``` r
# how man taxa in 'Easplist'
count_taxa(Easplist)
#> [1] 3887

# frequency of life forms
count_taxa(~ lf_behn_2018, Easplist)
#>          lf_behn_2018 taxa_count
#> 1    acropleustophyte          8
#> 2         chamaephyte         25
#> 3      climbing_plant         25
#> 4  facultative_annual         20
#> 5     obligate_annual        114
#> 6        phanerophyte         26
#> 7    pleustohelophyte          8
#> 8          reed_plant         14
#> 9       reptant_plant         19
#> 10      tussock_plant         52
```

Furthermore, taxonomic information can be also transferred to this slot
using the function `tax2traits()`. By this way we will make taxonomic
ranks suitable for frequency calculations.

``` r
Easplist <- tax2traits(Easplist, get_names=TRUE)
head(Easplist@taxonTraits)
#>    TaxonConceptID       lf_behn_2018 form variety subspecies
#> 3               7       phanerophyte <NA>    <NA>       <NA>
#> 4               9       phanerophyte <NA>    <NA>       <NA>
#> 7              18 facultative_annual <NA>    <NA>       <NA>
#> 8              20 facultative_annual <NA>    <NA>       <NA>
#> 9              21    obligate_annual <NA>    <NA>       <NA>
#> 14             22        chamaephyte <NA>    <NA>       <NA>
#>                   species complex        genus        family
#> 3         Acacia mearnsii    <NA>       Acacia   Leguminosae
#> 4      Acacia polyacantha    <NA>       Acacia   Leguminosae
#> 7      Achyranthes aspera    <NA>  Achyranthes Amaranthaceae
#> 8      Acmella caulirhiza    <NA>      Acmella    Compositae
#> 9       Acmella uliginosa    <NA>      Acmella    Compositae
#> 14 Aeschynomene schimperi    <NA> Aeschynomene   Leguminosae
```

Note that the respective parental ranks are inserted in the table
**taxonTraits**, which contains the attributes of the taxa. In the two
next command lines, we will produce a subset with only members of the
family Cyperaceae and then calculate the frequency of species per
genera.

``` r
Cype <- subset(Easplist, family == "Cyperaceae", slot="taxonTraits")
Cype_stat <- count_taxa(species ~ genus, Cype)
```

Now, we can sort them to produce a nice bar plot.

``` r
Cype_stat <- Cype_stat[order(Cype_stat$species_count, decreasing=TRUE),]

par(las=2, mar=c(10,3,1,1))
with(Cype_stat, barplot(species_count, names.arg=genus))
```

![](README-figures/genera_bar-1.png)<!-- -->

## Acknowledgements

The author thanks **Stephan Hennekens**, developer of
[Turboveg](http://www.synbiosys.alterra.nl/turboveg), for his patience
and great support finding a common language between **R** and
**Turboveg**, as well as for his advices on formatting our taxonomic
list **EA-Splist**.

Also thanks to **Federico Luebert** for the fruitful discussions
regarding the terminology used in this project.
