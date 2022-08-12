# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(taxlist)
library(readODS)

df <- list()
for(i in c("main", "taxon_traits", "taxon_views"))
  df[[i]] <- read_ods("lab/syntax-alvarez2017.ods", i)

x = df$main
levels = c("association", "alliance", "order", "class")

x <- x$TaxonName

Test <- df2taxlist(x)
