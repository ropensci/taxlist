## A subset with Pseudognaphalium and relatives
Pseudognaphalium <- subset(x = Easplist, subset = grepl("Pseudognaphalium",
        TaxonName), slot = "names")
Pseudognaphalium <- get_parents(Easplist, Pseudognaphalium)

## Create a backup with date stamp in tempdir
backup_object(Pseudognaphalium, file = file.path(tempdir(), "Pseudonaphalium"))

## The same
backup_object(Pseudognaphalium, file = file.path(tempdir(), "Pseudonaphalium"))

## To load the last backup into a session
load_last(file = file.path(tempdir(), "Pseudonaphalium"))

## Load pre-installed backup
load_last(file.path(path.package("taxlist"), "extdata", "Podocarpus"))
