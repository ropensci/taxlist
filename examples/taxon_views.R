## See existing views
taxon_views(Easplist)

## Add a new view
sp_list <- add_view(taxlist = Easplist, taxonViews = data.frame(
        secundum = "Beentje et al. (1952)",
        Title = "Flora of Tropical East Africa",
        URL = "http://www.kew.org/science/directory/projects/FloraTropEAfrica.html"))

taxon_views(sp_list)
