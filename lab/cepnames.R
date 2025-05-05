library(taxlist)
library(vegan)

# Copy taxonomic list
sp_list <- Easplist

# Make CEP-names for taxa
taxa <- accepted_name(sp_list)
taxa$sp_code <- make.cepnames(taxa$TaxonName)

# Make CEP-names for taxon usages
all_names <- sp_list@taxonNames
all_names$name_code <- make.cepnames(all_names$TaxonName)

# Set CEP-names as codes
reindex(sp_list, old = taxa$TaxonConceptID) <- taxa$sp_code
reindex(sp_list, idx = "usage", old = all_names$TaxonUsageID) <-
            all_names$name_code

# Check some species (Note the codes)
summary(sp_list, "papyrus")

# Pass the taxonomic list to taxnames ----
library(taxnames)
set_tax(sp_list)

# Print Cyperus papyrus name
tn_fn("Cypepapy")
tn_fna("Cypepapy")

# Print the name of the parents
tn_pfna("Cypepapy", "genus")
tn_pfna("Cypepapy", "family", italics = FALSE)
