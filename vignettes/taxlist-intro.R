## ----install_github, eval=FALSE-----------------------------------------------
## library(devtools)
## install_github("ropensci/taxlist", build_vignettes = TRUE)


## ----install_cran, eval=FALSE-------------------------------------------------
## install.packages("taxlist", dependencies = TRUE)


## ----load_taxlist, message=FALSE----------------------------------------------
library(taxlist)


## ----call_vignette, eval=FALSE------------------------------------------------
## vignette("taxlist-intro")


## ----load_example_table-------------------------------------------------------
load(file.path(path.package("taxlist"), "Cross.rda"))


## ----head_example-------------------------------------------------------------
head(Cross[, 1:8])


## ----character2taxlist--------------------------------------------------------
sp_list <- Cross[, "TaxonName"]
sp_list <- df2taxlist(x = sp_list)
summary(sp_list)


## ----summary_character--------------------------------------------------------
summary(object = sp_list, ConceptID = "Erigeron floribundus")


## ----load_easplist------------------------------------------------------------
data(Easplist)
Easplist


## ----summary_life_forms-------------------------------------------------------
summary(as.factor(Easplist$life_form))


## ----papyrus_otp1, results="hide"---------------------------------------------
Papyrus <- subset(x = Easplist, subset = grepl("papyrus", TaxonName), slot = "names")
summary(Papyrus, "all")


## ----papyrus_opt2, results="hide"---------------------------------------------
Papyrus <- subset(x = Easplist, subset = TaxonConceptID == 206, slot = "relations")
summary(Papyrus, "all")


## ----phragmites, results="hide"-----------------------------------------------
Phraaus <- subset(
  x = Easplist,
  subset = charmatch("Phragmites australis", TaxonName), slot = "names"
)
summary(Phraaus, "all")


## ----summary_again------------------------------------------------------------
Easplist


## ----recover_parents----------------------------------------------------------
summary(Papyrus, "all")
Papyrus <- get_parents(Easplist, Papyrus)
summary(Papyrus, "all")


## ----indented_list------------------------------------------------------------
indented_list(Papyrus)


## ----load_syntax--------------------------------------------------------------
load(file.path(path.package("taxlist"), "wetlands_syntax.rda"))


## ----prototype----------------------------------------------------------------
head(Concepts)

Concepts$TaxonUsageID <- Concepts$TaxonConceptID

Syntax <- df2taxlist(Concepts)

levels(Syntax) <- c("association", "alliance", "order", "class")

taxon_views(Syntax) <- data.frame(
  ViewID = 1, Secundum = "Alvarez (2017)",
  Author = "Alvarez M", Year = 2017,
  Title = "Classification of aquatic and semi-aquatic vegetation in East Africa",
  stringsAsFactors = FALSE
)

Syntax@taxonRelations$ViewID <- 1

Syntax


## ----adding_synonyms----------------------------------------------------------
head(Synonyms)
Syntax <- add_synonym(Syntax,
  ConceptID = Synonyms$TaxonConceptID,
  TaxonName = Synonyms$TaxonName, AuthorName = Synonyms$AuthorName
)


## ----adding_traits------------------------------------------------------------
head(Codes)
taxon_traits(Syntax) <- Codes
Syntax


## ----get_nymplot--------------------------------------------------------------
Nymplot <- subset(Syntax, charmatch("Nymphaeetum", TaxonName), slot = "names")
summary(Nymplot, "all")


## ----get_nymplot_2------------------------------------------------------------
Nymplot <- subset(Syntax, charmatch("Nymphaeetum", TaxonName),
  slot = "names",
  keep_parents = TRUE
)
summary(Nymplot, "all")
indented_list(Nymplot)

