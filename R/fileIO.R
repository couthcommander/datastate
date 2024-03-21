#' Turn Specification into YAML
#'
#' Convert a given a specification list into YAML.
#'
#' @param dd Specification list; this describes the desired state of the data set.
#' @param file YAML file name; defaults to standard output.
#'
#' @examples
#' data(ChickWeight)
#' spec <- datadict(ChickWeight)
#' spec$variables[[1]]$units <- 'gm'
#' spec$variables[[2]]$label <- 'time since birth'
#' spec$variables[[2]]$units <- 'days'
#' spec$variables[[4]]$label <- 'protein diet'
#' spec$variables[[3]] <- NULL
#' dd2yaml(spec)
#'
#' @aliases yaml2dd
#' @export

dd2yaml <- function(dd, file = stdout()) {
  yaml::write_yaml(dd, file)
}

yaml2dd <- function(file) {
  yaml::yaml.load_file(file)
}
