## A subset with Pseudognaphalium and relatives
Pseudognaphalium <- subset(x = Easplist, subset = grepl("Pseudognaphalium",
        TaxonName), slot = "names", keep_parents = TRUE)

## Create a backup with date stamp in tempdir
backup_object(Pseudognaphalium, file = file.path(tempdir(), "Pseudognaphalium"))

## Retrieve paths of backup
info_back <- backup_object(Pseudognaphalium, file = file.path(tempdir(),
        "Pseudognaphalium"))
info_back

## Display all backups
sort_backups("Pseudognaphalium", tempdir())

## Delete object
rm(list = "Pseudognaphalium")

## To load the last backup into a session
load_last("Pseudognaphalium", path = tempdir())

## Load pre-installed backup
load_last(file.path(path.package("taxlist"), "extdata", "Podocarpus"))
