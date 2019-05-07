################################################################################
#
# Remove variable labels for vectors or the whole data frame.
#
################################################################################
#' @param x A vector or data frame.
#' @family zappers
#' @export
#' @examples 
#' path <- system.file("examples", "iris.dta", package = "haven")
#' iris_dta <- read_dta(path)
#' 
#' # Show variable labels
#' lapply(iris_dta, attr, "label")
#' 
#' # Strip variable labels
#' iris_dta <- ZapLabel(iris_dta)
#' lapply(iris_dta, attr, "label")
ZapLabel <- function(x) {
  UseMethod("ZapLabel")
}

#' @export
ZapLabel.default <- function(x) {
  attr(x, "label") <- NULL
  x
}

#' @export
ZapLabel.data.frame <- function(x) {
  x[] <- lapply(x, ZapLabel)
  x
}