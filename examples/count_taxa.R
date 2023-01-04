## factor method
count_taxa(iris$Species)

## taxlist method
count_taxa(Easplist)

## count only species
count_taxa(Easplist, level = "species")

## using a formula
count_taxa(~life_form, Easplist, include_na = TRUE)
