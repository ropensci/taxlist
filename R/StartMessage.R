# TODO:   Message displayed on start (modified from vegdata)
# 
# Author: Miguel Alvarez
################################################################################

.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is taxlist ",
            utils::packageDescription("taxlist", field="Version"),
            appendLF=TRUE)
}
