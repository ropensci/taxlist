library(taxlist)

# Using charcter values
t_names <- c("Poa annua", "Triticum aestivum", "Brassica rapa")

print_name(t_names)
print_name(t_names, style = "html")
print_name(t_names, style = "knitr")
print_name(t_names, style = "expression")

print_name(t_names, italics = FALSE)
print_name(t_names, style = "html", italics = FALSE)
print_name(t_names, style = "knitr", italics = FALSE)
print_name(t_names, style = "expression", italics = FALSE)

print_name(t_names, collapse = c(", ", ", and "))
print_name(t_names, style = "html", collapse = c(", ", ", and "))
print_name(t_names, style = "knitr", collapse = c(", ", ", and "))
print_name(t_names, style = "expression", collapse = c(", ", ", and "))

# Using taxlist objects
t_names <- c(339, 340, 50499)

print_name(Easplist, t_names)
print_name(Easplist, t_names, style = "html")
print_name(Easplist, t_names, style = "knitr")
print_name(Easplist, t_names, style = "expression")

print_name(Easplist, t_names, italics = FALSE)
print_name(Easplist, t_names, style = "html", italics = FALSE)
print_name(Easplist, t_names, style = "knitr", italics = FALSE)
print_name(Easplist, t_names, style = "expression", italics = FALSE) # Error!

print_name(Easplist, t_names, collapse = c(", ", ", and "))
print_name(Easplist, t_names, style = "html", collapse = c(", ", ", and "))
print_name(Easplist, t_names, style = "knitr", collapse = c(", ", ", and "))
print_name(Easplist, t_names, style = "expression",
    collapse = c(", ", ", and "))
