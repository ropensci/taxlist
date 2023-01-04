## Coerce taxlist to list
tax_list <- as(Easplist, "list")

## Coerce data frame to taxlist
Cyperus <- read.csv(file = file.path(path.package("taxlist"), "cyperus",
        "names.csv"))
Cyperus$AcceptedName <- !Cyperus$SYNONYM
head(Cyperus)

as(Cyperus, "taxlist")
