#' Internal functions for datastate
#'
#' @name datastate-internal
#' @aliases thing2string varCode
#' @keywords internal
NULL

thing2string <- function(value) {
  paste(utils::capture.output(dput(value)), collapse = '\n')
}

varCode <- function(value, name = NULL) {
  if(is.null(name)) name <- as.character(substitute(value))
  sprintf('%s <- %s', name, thing2string(value))
}
