## Produce a data set with only reed plants
sp_list <- subset(x = Easplist, subset = life_form == "reed_plant",
    slot = "taxonTraits", keep_parents = TRUE)
sp_list

summary(as.factor(sp_list$life_form))
