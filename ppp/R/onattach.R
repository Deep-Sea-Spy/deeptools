#' @title .onAttach
#' @noRd
.onAttach <- function(libname, pkgname) {
  message("Read the report with deeptools::open_guide()")
  message("Open the HTML documentation with deeptools::open_pkgdown()")
}
