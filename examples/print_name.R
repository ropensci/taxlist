## Example subspecies
summary(Easplist, 363, secundum = "secundum")

## Empty plot
plot(x = NA, xlim = c(0, 5), ylim = c(7, 1), bty = "n", xaxt = "n", xlab = "",
  ylab = "options")

## Accepted name with author
text(x = 0, y = 1, labels = print_name(Easplist, 363, style = "expression"),
  pos = 4)

## Including taxon view
text(x = 0, y = 2, labels = print_name(Easplist, 363, style = "expression",
  secundum = "secundum"), pos = 4, cex = 0.7)

## Second mention in text
text(x = 0, y = 3, labels = print_name(Easplist, 363, style = "expression",
  second_mention = TRUE), pos = 4)

## Using synonym
text(x = 0, y = 4, labels = print_name(Easplist, 50037, style = "expression",
  concept = FALSE), pos = 4)

## Markdown style
text(0, 5, labels = print_name(Easplist, 363, style = "markdown"), pos = 4)

## HTML style
text(0, 6, labels = print_name(Easplist, 363, style = "html"), pos = 4,
    cex = 0.7)

## LaTeX style for knitr
text(x = 0, y = 7, labels = print_name(Easplist, 363, style = "knitr"), pos = 4,
    cex = 0.7)
