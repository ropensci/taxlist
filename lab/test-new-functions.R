# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

# Load package
library(taxlist)

# Load data
load(file.path(path.package("taxlist"), "wetlands_syntax.rda"))

# Build taxlist
Syntax <- df2taxlist(Concepts,
    levels = c("association", "alliance", "order", "class"))
Syntax
