## Set a different accepted name for Cyclosorus interruptus
summary(Easplist, "Cyclosorus interruptus")
accepted_name(Easplist, 50074) <- 53097
summary(Easplist, 50074)

## Inserting a new name first
summary(Easplist, "Basella alba")
Easplist <- add_synonym(taxlist = Easplist, ConceptID = 68,
    TaxonName = "Basella cordifolia", AuthorName = "Lam.")
summary(Easplist, 68)
accepted_name(Easplist, 68) <- 56139
summary(Easplist, 68)

## Display synonyms
head(synonyms(taxlist = Easplist))

## Synonyms for an specific concept
synonyms(taxlist = Easplist, ConceptID = 20)

## Basionym for Cyclosrus interruptus
summary(Easplist, 50074)
basionym(Easplist, 50074) <- 53097

summary(Easplist, 50074)
basionym(Easplist, 50074)

## Move the name Typha aethiopica to concept 573 (T. latifolia)
summary(Easplist, c(50105, 573))
change_concept(Easplist, 53130) <- 573
summary(Easplist, c(50105, 573))
