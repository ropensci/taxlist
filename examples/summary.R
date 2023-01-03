## summary of the object
summary(Easplist, units = "Mb")

## the same output
summary(Easplist)
show(Easplist)
print(Easplist)
Easplist

## summary for two taxa
summary(Easplist, c(51128, 51140))

## summary by matching a name
summary(Easplist, "Acmella")

## summary for the first 10 taxa
summary(object = Easplist, ConceptID = "all", maxsum = 10)
