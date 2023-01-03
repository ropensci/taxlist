## Direct manipulation of slot taxonRelations generates an invalid object
Easplist@taxonRelations <- Easplist@taxonRelations[1:5, ]

## Now apply cleaning
Easplist <- clean(Easplist)
summary(Easplist)
