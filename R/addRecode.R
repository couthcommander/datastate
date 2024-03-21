#' Update Specification with Recoded Variables
#'
#' Create a specification list with \code{datadict}. Then apply this
#' function to prepare for recoded variables.
#'
#' @param dd Specification list; this describes the desired state of the data set.
#' @param dat data.frame to be modified; when missing will be determined from specification.
#' @param vars character vector or list; column names that should be recoded. If in list form, elements
#' should be individual values to recode for given column names.
#'
#' @return Updated specification list
#'
#' @examples
#' data(airquality)
#' spec <- datadict(airquality)
#' upspec <- addRecode(spec, vars = list(Month = c(5,6,7,8,9)))
#'
#' @export

addRecode <- function(dd, dat, vars) {
  if(missing(vars)) {
    stop('Please specify the vars argument. It should be one or more column names, or a list with values to recode.')
  }
  datname <- dd$name
  if(missing(dat)) dat <- get(datname)
  cnames <- names(dat)
  dnames <- varNames(dd)
  if(is.list(vars)) {
    vals <- vars
    vars <- names(vars)
  } else {
    vals <- NULL
  }
  stopifnot(all(dnames %in% cnames), all(vars %in% dnames))
  dloc <- match(vars, dnames)
  for(i in seq_along(vars)) {
    if('factor' %in% names(dd$variables[[dloc[i]]])) {
      # don't use this on factors
      warning(sprintf("ignoring factor variable, %s", vars[i]))
      next
    }
    di <- dat[[vars[i]]]
    if(is.factor(di)) {
      vi <- levels(di)
      warning(sprintf("ignoring factor variable, %s", vars[i]))
      next
    } else {
      vi <- sort(unique(di))
    }
    vx <- vi %in% vals[[i]]
    if(any(vx)) {
      vi <- vi[vx]
    }
    rc <- lapply(vi, function(i) list(oldvalue = i, newvalue = ''))
    dd$variables[[dloc[i]]]$recode <- rc
  }
  dd
}
