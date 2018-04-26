Current
=======

### New Features

* New function `load_last` to load last backup in an R-session.
* File **inst/ChangeLog** replaced by **NEWS.md**.
* New column `Secundum` as mandatory field in slot `taxonViews`.

### Improvements

* Function `backup_object` is also working with relative paths.

### Bugs Fixes

* Function `add_view` was not adding new columns in the respective slot.

taxlist 0.1.3
=============

### New Features

* New function: `add_trait`.
* New function: `tax2traits`.

### Improvements

* Argument `level` inserted in function `merge_taxa`.
* Function `clean` also set keys to class `integer`.
* Validation checks for the existence of accepted names in names list.

### Bugs Fixes

* Bug in `add_concept`: wrong assignment of `AcceptedName`.

taxlist 0.1.2
=============

### New Features

* new function `merge_taxa`.

### Improvements

* Argument `ConceptID` in `summary` (`taxlis-method`) can be a character vector matching `TaxonName`.

taxlist 0.1.1
=============

### New Features

* New vignette `taxlist-intro`.

### Improvements

* Package `vegdata` moved from Depends to Imports.
* Function `df2taxlist` adapted to species lists with duplicated names.
* Arguments `keep_parents` and `keep_children` implemented in function `subset`.

taxlist 0.1.0
=============

### New Features

* Released to **CRAN** (https://cran.r-project.org/package=taxlist).

taxlist 0.0.0.9014
==================

### New Features

* New method `as.list`.

### Improvements

* New validation rule: duplicated combinations `TaxonName` and `AuthorName` not allowed in slot `taxonNames`.
* Data **Easplist** modified to fulfil the latter rule.
