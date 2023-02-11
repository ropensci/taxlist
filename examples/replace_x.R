## Replace values in vector
replace_x(x = letters, old = c("b", "p", "f"), new = c("bee", "pork", "fungus"))

## Replace values using indices
replace_idx(x = letters, idx1 = 1:length(letters), idx2 = c(2, 7, 17),
  new = c("second", "seventh", "seventeenth"))

## Replace values if they are NAs
letters[2] <- NA
replace_na(x = letters, idx1 = 1:length(letters), idx2 = c(1:3),
  new = c("alpha", "beta", "zeta"))

## The same applications but this time for functional traits
summary(as.factor(Easplist$life_form))

# Merge annuals
Easplist@taxonTraits$lifeform <- replace_x(x = Easplist@taxonTraits$life_form,
  old = c("obligate_annual", "facultative_annual"), new = c("annual", "annual"))
summary(as.factor(Easplist$lifeform))

# The same effect
Easplist@taxonTraits$lifeform <- replace_idx(x = Easplist@taxonTraits$life_form,
  idx1 = grepl("annual", Easplist@taxonTraits$life_form), idx2 = TRUE,
  new = "annual")
summary(as.factor(Easplist$lifeform))

## Merge data frames including new columns
data(iris)
iris$Species <- paste(iris$Species)
new_iris <- data.frame(Species = rep("humilis", 2), Height = c(15, 20),
  stringsAsFactors = FALSE)
insert_rows(iris, new_iris)
