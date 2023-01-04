## Merge Cyperus papyrus and Cyperus dives
summary(Easplist, c(206, 197))

sp_list <- merge_taxa(object = Easplist, concepts = c(206, 197),
    print_output = TRUE)

## Move the name Typha aethiopica to concept 573 (T. latifolia)
summary(Easplist, c(50105, 573))
change_concept(Easplist, 53130) <- 573
summary(Easplist, c(50105, 573))
