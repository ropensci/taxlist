## ----install_github, eval=FALSE-----------------------------------------------
## library(devtools)
## install_github("kamapu/taxlist", build_vignettes=TRUE)


## ----install_cran, eval=FALSE-------------------------------------------------
## install.packages("taxlist", dependencies=TRUE)


## ----load_taxlist, message=FALSE----------------------------------------------
library(taxlist)


## ----call_vignette, eval=FALSE------------------------------------------------
## vignette("taxlist-intro")


## ----load_example_table-------------------------------------------------------
load(url("https://github.com/kamapu/thisdataismine/raw/master/data/Cross.rda"))


## ----head_example-------------------------------------------------------------
head(Cross[ ,1:8])


## ----character2taxlist--------------------------------------------------------
Splist <- Cross[ ,"TaxonName"]
Splist <- df2taxlist(Splist)
summary(Splist)


## ----summary_character--------------------------------------------------------
summary(Splist, "Erigeron floribundus")


## ----load_easplist------------------------------------------------------------
data(Easplist)
Easplist


## ----summary_life_forms-------------------------------------------------------
summary(as.factor(Easplist$lf_behn_2018))


## ----papyrus_otp1, results="hide"---------------------------------------------
Papyrus <- subset(x=Easplist, subset=grepl("papyrus", TaxonName), slot="names")
summary(Papyrus, "all")


## ----papyrus_opt2, results="hide"---------------------------------------------
Papyrus <- subset(x=Easplist, subset=TaxonConceptID == 206, slot="relations")
summary(Papyrus, "all")


## ----phragmites, results="hide"-----------------------------------------------
Phraaus <- subset(x=Easplist,
		subset=charmatch("Phragmites australis", TaxonName), slot="names")
summary(Phraaus, "all")


## ----summary_again------------------------------------------------------------
Easplist


## ----recover_parents----------------------------------------------------------
summary(Papyrus, "all")
Papyrus <- get_parents(Easplist, Papyrus)
summary(Papyrus, "all")


## ----load_syntax--------------------------------------------------------------
load(url("https://github.com/kamapu/Guides/raw/master/data/wetlands_syntax.rda"))


## ----prototype----------------------------------------------------------------
head(Concepts)

Syntax <- new("taxlist")

levels(Syntax) <- c("association","alliance","order","class")

taxon_views(Syntax) <- data.frame(ViewID=1, Secundum="Alvarez (2017)",
		Author="Alvarez M", Year=2017,
        Title="Classification of aquatic and semi-aquatic vegetation in East Africa",
        stringsAsFactors=FALSE)

Syntax <- add_concept(Syntax, TaxonName=Concepts$TaxonName,
		AuthorName=Concepts$AuthorName, Parent=Concepts$Parent,
		Level=Concepts$Level, ViewID=rep(1, nrow(Concepts)))

Syntax


## ----adding_synonyms----------------------------------------------------------
head(Synonyms)
Syntax <- add_synonym(Syntax, ConceptID=Synonyms$TaxonConceptID,
		TaxonName=Synonyms$TaxonName, AuthorName=Synonyms$AuthorName)


## ----adding_traits------------------------------------------------------------
head(Codes)
taxon_traits(Syntax) <- Codes
Syntax


## ----get_nymplot--------------------------------------------------------------
Nymplot <- subset(Syntax, charmatch("Nymphaeetum", TaxonName), slot="names")
summary(Nymplot, "all")


## ----get_nymplot_2------------------------------------------------------------
Nymplot <- subset(Syntax, charmatch("Nymphaeetum", TaxonName), slot="names",
	keep_parents=TRUE)
summary(Nymplot, "all")

