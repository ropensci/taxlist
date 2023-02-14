## Creating numeric IDs
id_generator(len = 10, minvalue = 5)

## Creating character IDs
id_generator(len = 10, mode = "character")

## Solving duplicates in numeric identifiers
id_solver(insert = c(3, 7, 5, 10), to = c(1:5))

## Solving duplicates in bibtexkeys
db_refs <- c("Alvarez2003", "Schmitz1988", "Li2023")
new_refs <- c("Alvarez2003", "Li2023", "Mueller1953", "Alvarez2003a")
any(duplicated(c(db_refs, new_refs)))

solved_refs <- id_solver(insert = new_refs, to = db_refs, suffix = "character")
solved_refs
any(duplicated(c(db_refs, solved_refs)))
