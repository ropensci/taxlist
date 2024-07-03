context("printing taxon usage names")

test_that("printed names are working", {
  expect_is(
    print_name(Easplist, 363, style = "expression"),
    "expression"
  )
  expect_is(print_name(Easplist, 363,
    style = "expression",
    secundum = "secundum"
  ), "expression")
  expect_equal(
    paste(print_name(Easplist, 363)),
    "*Ludwigia adscendens* ssp. *diffusa* (Forssk.) P.H. Raven"
  )
  expect_equal(
    paste(print_name(Easplist, 363, style = "html")),
    paste(
      "<i>Ludwigia adscendens</i> ssp. <i>diffusa</i>",
      "(Forssk.) P.H. Raven"
    )
  )
  expect_equal(
    paste(print_name(Easplist, 363, style = "knitr")),
    paste(
      "\\textit{Ludwigia adscendens} ssp.",
      "\\textit{diffusa} (Forssk.) P.H. Raven"
    )
  )
  ## when more than one name is requested
  ## expect_warning(print_name(Easplist, c(1, 2)))
})

test_that("name strings get collapsed", {
  t_names <- c(339, 340, 50499)
  expect_equal(
    length(print_name(Easplist, t_names, collapse = c(", ", ", and "))),
    1
  )
  expect_equal(
    length(print_name(Easplist, t_names,
      style = "html",
      collapse = c(", ", ", and ")
    )),
    1
  )
  expect_equal(
    length(print_name(Easplist, t_names,
      style = "knitr",
      collapse = c(", ", ", and ")
    )),
    1
  )
  expect_equal(
    length(print_name(Easplist, t_names,
      secundum = "secundum",
      second_mention = TRUE, collapse = c(" / ")
    )),
    1
  )
})

test_that("error messages are triggered", {
  expect_error(print_name("Poa annua", style = "latex"))
  t_names <- c(339, 340, 50499)
  expect_error(print_name(Easplist, t_names, secundum = "some_column"))
})
