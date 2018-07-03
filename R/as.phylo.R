# TODO:   Convert taxlist objects into phylo class
# 
# Author: Miguel Alvarez
################################################################################

# Generic method
if(!isGeneric("as.phylo"))
	setGeneric("as.phylo",
			function(x, ...)
				standardGeneric("as.phylo")
	)

# Method for taxlist
setMethod("as.phylo", signature(x="taxlist"),
		function(x, print_name=FALSE, include_author=FALSE,
				second_mention=FALSE, ...) {
			if(sum(is.na(x@taxonRelations$Parent)) > 1)
				stop("Multiple trees detected. Use 'as.multiPhylo' instead.")
			TR <- x@taxonRelations[,c("Parent","TaxonConceptID","Level")]
			TR$node <- with(TR, TaxonConceptID %in% Parent)
			nr_nodes <- sum(TR$node)
			nr_tips <- sum(!TR$node)
			# Edge
			edge <- as.matrix(TR[,c("Parent","TaxonConceptID")])
			edge[!TR$node,2] <- 1:nr_tips
			edge[TR$node,2] <- nr_tips + (1:nr_nodes)
			edge[,1] <- edge[match(edge[,1], TR$TaxonConceptID),2]
			colnames(edge) <- NULL
			rownames(edge) <- NULL
			# Calculate edge length
			edge_length <- TR[,c("TaxonConceptID","Parent")]
			for(i in colnames(edge_length))
				edge_length[,i] <- with(x@taxonRelations,
						as.numeric(Level)[match(edge_length[,i], TaxonConceptID)])
			edge_length <- with(edge_length, Parent - TaxonConceptID)
			# Taxon names
			tax_names <- with(x@taxonRelations,
					AcceptedName[match(TR$TaxonConceptID, TaxonConceptID)])
			if(print_name) {
				tax_names <- split(tax_names, 1:length(tax_names))
				for(i in 1:length(tax_names)) {
					tax_names[[i]] <- print_name(x, tax_names[[i]], concept=FALSE,
							second_mention=second_mention,
							include_author=include_author,
							style="expression")
				}
				tax_names <- unsplit(tax_names, 1:length(tax_names))
			} else {
				tax_names <- x@taxonNames[match(tax_names,
								x@taxonNames$TaxonUsageID), c("TaxonName",
								"AuthorName")]
				if(include_author)
					tax_names <- paste(tax_names[,1], tax_names[,1]) else
					tax_names <- tax_names[,1]
			}
			# New object
			TR <- list(edge=edge[!is.na(edge[,1]),], Nnode=as.integer(nr_nodes),
					tip.label=1:nr_tips)
			class(TR) <- "phylo"
			TR$tip.label <- tax_names[match(1:nr_tips, edge[,2])]
			TR$edge.length <- edge_length[!is.na(edge[,1])]
			TR$node.label <- tax_names[match(nr_tips + (1:nr_nodes), edge[,2])]
			return(TR)
		}
)
