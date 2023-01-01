## Names to be compared
species <- c("Cyperus papyrus", "Typha australis", "Luke Skywalker")

## Comparing character vectors
match_names(c("Cyperus paper", "TIE fighter"), species)

## Retrieve taxon usage names
match_names(species, Easplist)

## Display accepted names in output
match_names(x = species, object = Easplist, show_concepts = TRUE)

# Using cut value for similarity
match_names(x = species, object = Easplist, cutlevel = 0.8)
