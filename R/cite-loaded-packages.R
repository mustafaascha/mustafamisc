







#' Cite all loaded packages
#'
#' @return
#' @export
#'
#' @examples
#' library(Hmisc)
#' bib_loaded_packages()
#'
bib_loaded_packages <- function(){
  lp <- loaded_packages <- search()
  lp <- lp[grepl("package", lp)]
  lp <- gsub("package:", "", lp)
  lapply(lp, citation)
}
