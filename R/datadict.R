#' Create Specification List
#'
#' A specification list contains information about a data set such as variables and its labels.
#' It can be modified and passed to \code{transform} to make changes to the data.
#'
#' @param dat data.frame
#'
#' @return list
#'
#' @examples
#' data(ChickWeight)
#' spec <- datadict(ChickWeight)
#'
#' @export

datadict <- function(dat) {
  cnames <- names(dat)
  vlist <- vector('list', length(cnames))
  for(i in seq_along(cnames)) {
    di <- dat[[i]]
    ai <- attributes(di)
    vl <- list(name = cnames[i], label = '', units = '')
    if('label' %in% names(ai)) vl$label <- ai$label
    if('units' %in% names(ai)) vl$units <- ai$units
    if(is.factor(di)) {
      li <- lapply(levels(di), function(i) list(label_output = i, level_input = i))
      if(is.ordered(di)) {
        vl$ordered_factor <- li
      } else {
        vl$factor <- li
      }
    }
    vlist[[i]] <- vl
  }
  list(name = as.character(substitute(dat)), variables = vlist)
}
