#' Installs packages, if necessary, and loads them.
#' 
#'  Nice to reproducibility when sharing files which depend on packages with colleagues.
#'  @param ... Name of packages. Can be given as characters strings or sans-quotes.
#'  @export
#'  @examples
#'  require_packages(knitr, formatR)
#'  
#'  ## Not run:
#'  #  Will result in error
#'  \dontrun{require_packages(knitr, formatR, not_a_real_package)}
#'  ## End(**Not run**)
#'  
#'  ### OTHER INPUT FORMATS:  ###
#' require_packages(c("knitr", "formatR"))
#' require_packages("knitr", "formatR")
require_packages <- function(...)
{
    #  Figure out if ... is a character string or just names
    input <- structure(as.list(match.call()[-1]), class = "quoted")
    if(class(input[[1]]) == "call"){
        packs <- eval(input[[1]])
    } else {
        packs <- as.character(match.call())[-1]
    }
    load_packages <- sapply(packs, function(a_package) {
        if (!(a_package %in% rownames(installed.packages()))) {
            install.packages(a_package)
        }
        require(a_package,
                quietly = TRUE,
                character.only = TRUE,
                warn.conflicts = FALSE)
    })
    missing_packages <- paste(packs[!load_packages], collapse = ", ")
    if(all(load_packages) == FALSE)
        stop(paste("\nThe packages:",
                   missing_packages,
                   "could be loaded or found on CRAN.",
                   "Please check spelling or package availablility."))
}
