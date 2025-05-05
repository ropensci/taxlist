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
indented_list(Fern, alphabetical = TRUE)

# Test sorting names in advance
Fern@taxonNames <- Fern@taxonNames[order(Fern@taxonNames$TaxonName,
  decreasing = TRUE
), ]
Fern@taxonRelations <- Fern@taxonRelations[match(Fern@taxonNames$TaxonUsageID, Fern@taxonRelations$AcceptedName), ]

indented_list(Fern)




levels(Fern)
head(Fern@taxonRelations)
