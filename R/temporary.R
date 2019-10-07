#' Check tidyr version
#' 
#' @description Check the version of tidyr to handle the gap between v0.8.3 and v1.0.0
#' 
#' @return the package version of tidyr
#' 
#' @keywords internal
#' @export
tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}
