#' Variables Names in Given Specifications
#'
#' Given a specification list, return the variable names.
#'
#' @param dd Specification list; this describes the desired state of the data set.
#'
#' @return character vector
#'
#' @examples
#' data(ChickWeight)
#' spec <- datadict(ChickWeight)
#' varNames(spec)
#' spec$variables[[3]] <- NULL
#' varNames(spec)
#'
#' @export

varNames <- function(dd) {
  names(dd$variables)
}
