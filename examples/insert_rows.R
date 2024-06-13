## Merge data frames including new columns
data(iris)
iris$Species <- paste(iris$Species)
new_iris <- data.frame(Species = rep("humilis", 2), Height = c(15, 20),
    stringsAsFactors = FALSE)
insert_rows(iris, new_iris)
