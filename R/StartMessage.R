.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is taxlist ",
            utils::packageDescription("taxlist", field="Version"),
            appendLF=TRUE)
}
