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
#' spec$variables[['weight']]$units <- 'gm'
#' spec$variables$Time$label <- 'time since birth'
#' spec$variables$Time$units <- 'days'
#' spec$variables$Diet$label <- 'protein diet'
#' spec$variables$Chick <- NULL
#' dd2yaml(spec)
#'
#' @export

dd2yaml <- function(dd, file = stdout()) {
  yaml::write_yaml(dd, file)
}

#' @rdname dd2yaml
#' @export
yaml2dd <- function(file) {
  yaml::yaml.load_file(file)
}
