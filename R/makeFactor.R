#' Update Specification with Factor Variables
#'
#' Create a specification list with \code{datadict}. Then apply this
#' function for proposed changes to factor variables.
#'
#' @param dd Specification list; this describes the desired state of the data set.
#' @param dat data.frame to be modified; when missing will be determined from specification.
#' @param vars character vector; column names that should be considered categorical.
#' @param ordered logical flag to determine if levels should be regarded as ordered. See \code{\link[base]{factor}}.
#'
#' @return Updated specification list
#'
#' @examples
#' data(airquality)
#' spec <- datadict(airquality)
#' upspec <- makeFactor(spec, vars = 'Month')
#'
#' @aliases makeOrderedFactor
#' @export

makeFactor <- function(dd, dat, vars, ordered = FALSE) {
  datname <- dd$name
  if(missing(dat)) dat <- get(datname)
  cnames <- names(dat)
  dnames <- varNames(dd)
  stopifnot(all(dnames %in% cnames), all(vars %in% dnames))
  factor_type <- c('factor', 'ordered_factor')[ordered+1]
  dloc <- match(vars, dnames)
  for(i in seq_along(vars)) {
    di <- dat[[vars[i]]]
    li <- lapply(sort(unique(di)), function(i) list(label_output = '', level_input = i))
    dd$variables[[dloc[i]]][[factor_type]] <- li
  }
  dd
}

makeOrderedFactor <- function(dd, dat, vars) {
  makeFactor(dd, dat, vars, ordered = TRUE)
}
