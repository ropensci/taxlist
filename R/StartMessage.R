.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is taxlist ",
            utils::packageDescription("taxlist", field="Version"),
            appendLF=TRUE)
	from_taxize <- c("apg_families","apg_orders","theplantlist")
	rm(list=ls()[ls() %in% from_taxize])
}
