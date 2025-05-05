# Load package and taxonomic list
library(taxlist)

# Generating data set
Fern <- data.frame(
  TaxonConceptID = 1:10,
  TaxonName = c(
    "Lycopodiopsida",
    "Lycopodiales",
    "Lycopodiaceae",
    "Lycopodielloideae",
    "Lycopodioideae",
    "Huperzioideae",
    "Isoëtales",
    "Isoëtaceae",
    "Selaginellales",
    "Selaginellaceae"
  ),
  AuthorName = c(
    "Bartl.",
    "DC. ex Bercht. & J.Presl",
    "P.Beauv.",
    rep("W.H.Wagner & Beitel ex B.Øllg.", 3),
    "Prantl",
    "Dumort.",
    "Prantl",
    "Willk"
  ),
  Level = c(
    "class",
    "order",
    "family",
    rep("subfamily", 3),
    rep(c("order", "family"), 2)
  ),
  Parent = c(NA, 1, 2, rep(3, 3), 1, 7, 1, 9)
)

Fern <- df2taxlist(Fern, levels = c(
  "subfamily",
  "family",
  "order",
  "class"
))

Fern

indented_list(Fern)

Fern <- sort_taxa(Fern, decreasing = TRUE)
indented_list(Fern)

Fern <- sort_taxa(Fern, priority = c("Selaginellales", "Lycopodioideae"))
indented_list(Fern)

## Example for sort_taxa
tax <- subset(Easplist,
  TaxonName %in% c("Boraginaceae"),
  keep_children = TRUE, keep_parents = TRUE
)
indented_list(tax)

tax <- sort_taxa(tax)
indented_list(tax)

tax <- sort_taxa(tax, priority = c("Euploca", "Myosotis", "Cordia monoica"))
indented_list(tax)



tax <- subset(Easplist, TaxonName == "Cordia",
  keep_children = TRUE,
  keep_parents = TRUE
)
summary(tax, "all")

tax <- sort_taxa(tax)
summary(tax, "all")
