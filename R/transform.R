#' Transform data.frame with Given Specifications
#'
#' Create a specification list with \code{datadict} in order to make modifications to a data set.
#'
#' @param dd Specification list; this describes the desired state of the data set.
#' @param dat data.frame to be modified; when missing will be determined from specification.
#' @param delete When TRUE data.frame variables not found in the specification will be removed.
#' @param modify Set to FALSE for trial run, and the data set will not be modified.
#' @param codefile file name to log code and comments with requested modifications; defaults to standard output.
#' @param Hmisc_label Label variables with \code{\link[Hmisc]{label}} if available.
#'
#' @return The modified data.frame, or original returned invisibly.
#'
#' @examples
#' data(ChickWeight)
#' spec <- datadict(ChickWeight)
#' spec$variables[[1]]$label <- 'body weight'
#' spec$variables[[1]]$units <- 'gm'
#' spec$variables[[2]]$label <- 'time since birth'
#' spec$variables[[2]]$units <- 'days'
#' spec$variables[[3]]$label <- 'chick identifier'
#' spec$variables[[4]]$label <- 'protein diet'
#' modCW <- transform(spec, Hmisc_label = FALSE)
#'
#' @export

transform <- function(dd, dat, delete = TRUE, modify = TRUE, codefile = '', Hmisc_label = TRUE) {
  datname <- dd$name
  if(missing(dat)) dat <- get(datname)
  cur <- datadict(dat)
  cur$name <- datname
  if(isTRUE(all.equal(dd, cur))) {
    cat('# no changes to make\n', file = codefile)
    if(modify) {
      return(dat)
    } else {
      return(invisible(dat))
    }
  }
  if(Hmisc_label && !requireNamespace("Hmisc", quietly = TRUE)) {
    Hmisc_label <- FALSE
  }
  dnames1 <- varNames(dd)
  dnames2 <- varNames(cur)
  col2er <- setdiff(dnames1, dnames2)
  if(length(col2er) > 0) {
    stop(sprintf('unaccounted variables: %s'), paste(col2er, collapse = ', '))
  }
  if(!delete) {
    col2rm <- character(0)
  } else {
    col2rm <- setdiff(dnames2, dnames1)
  }
  log <- character(0)
  for(i in seq_along(col2rm)) {
    if(modify) {
      dat[,col2rm[i]] <- NULL
    }
    log <- c(log, sprintf('# deleting variable [%s]', col2rm[i]))
    log <- c(log, sprintf("%s[,'%s'] <- NULL", datname, col2rm[i]))
  }
  cix <- match(dnames1, dnames2)
  for(i in seq_along(dnames1)) {
    v1 <- dd$variables[[i]]
    v2 <- cur$variables[[cix[i]]]
    if(isTRUE(all.equal(v1, v2))) next
    if(is.null(v1$label)) v1$label <- ''
    if(is.null(v2$label)) v2$label <- ''
    if(is.null(v1$units)) v1$units <- ''
    if(is.null(v2$units)) v2$units <- ''
    if(v1$label != v2$label) {
      log <- c(log, sprintf('# updating label in variable [%s]', dnames1[i]))
      if(v1$label == '') {
        if(modify) {
          class(dat[,dnames1[i]]) <- setdiff(class(dat[,dnames1[i]]), "labelled")
          attr(dat[,dnames1[i]], "label") <- NULL
        }
        log <- c(log, sprintf("class(%s[,'%s']) <- %s", datname, dnames1[i], setdiff(class(dat[,dnames1[i]]), "labelled")))
        log <- c(log, sprintf("attr(%s[,'%s'], 'label') <- NULL", datname, dnames1[i]))
      } else {
        if(modify) {
          if(Hmisc_label) {
            Hmisc::label(dat[,dnames1[i]]) <- v1$label
          } else {
            attr(dat[,dnames1[i]], "label") <- v1$label
          }
        }
        if(Hmisc_label) {
          log <- c(log, varCode(v1$label, 'vlabel'), sprintf("Hmisc::label(%s[,'%s']) <- vlabel", datname, dnames1[i]))
        } else {
          log <- c(log, varCode(v1$label, 'vlabel'), sprintf("attr(%s[,'%s'], 'label') <- vlabel", datname, dnames1[i]))
        }
      }
    }
    if(v1$units != v2$units) {
      log <- c(log, sprintf('# updating units in variable [%s]', dnames1[i]))
      if(v1$units == '') {
        if(modify) {
          attr(dat[,dnames1[i]], "units") <- NULL
        }
        log <- c(log, sprintf("attr(%s[,'%s'], 'units') <- NULL", datname, dnames1[i]))
      } else {
        if(modify) {
          attr(dat[,dnames1[i]], "units") <- v1$units
        }
        log <- c(log, varCode(v1$units, 'vunits'), sprintf("attr(%s[,'%s'], 'units') <- vunits", datname, dnames1[i]))
      }
    }
    # state:
    # 0 - non-factor
    # 1 - factor
    # 10 - ordered_factor
    # 11 - factor + ordered_factor... use min to convert to 10
    catstate1 <- min(10, 1 * ('factor' %in% names(v1)) + 10 * ('ordered_factor' %in% names(v1)))
    catstate2 <- min(10, 1 * ('factor' %in% names(v2)) + 10 * ('ordered_factor' %in% names(v2)))
    curattr <- attributes(dat[,dnames1[i]])
    catchange <- FALSE
    logmsg <- NULL
    codemsg <- NULL
    if(catstate1 == 0 && catstate2 == 0) {
      # do nothing
    } else if(catstate1 == 1) {
      if(catstate2 != 1 || !isTRUE(all.equal(v1$factor, v2$factor))) {
        newlab <- vapply(v1$factor, `[[`, character(1), 'label_output')
        newlev <- vapply(v1$factor, `[[`, character(1), 'level_input')
        if(modify) {
          fvar <- as.character(dat[,dnames1[i]])
          datm <- newlev[match(fvar, newlev)]
          dat[,dnames1[i]] <- factor(datm, newlev, newlab)
        }
        codemsg <- sprintf("%s
%s
fvar <- as.character(%s[,'%s'])
datm <- newlev[match(fvar, newlev)]
%s[,'%s'] <- factor(datm, newlev, newlab)
", varCode(newlab), varCode(newlev), datname, dnames1[i], datname, dnames1[i])
        catchange <- TRUE
      }
    } else if(catstate1 == 10) {
      if(catstate2 != 10 || !isTRUE(all.equal(v1$ordered_factor, v2$ordered_factor))) {
        newlab <- vapply(v1$ordered_factor, `[[`, character(1), 'label_output')
        newlev <- vapply(v1$ordered_factor, `[[`, character(1), 'level_input')
        if(modify) {
          fvar <- as.character(dat[,dnames1[i]])
          datm <- newlev[match(fvar, newlev)]
          dat[,dnames1[i]] <- factor(datm, newlev, newlab, ordered = TRUE)
        }
        codemsg <- sprintf("%s
%s
fvar <- as.character(%s[,'%s'])
datm <- newlev[match(fvar, newlev)]
%s[,'%s'] <- factor(datm, newlev, newlab, ordered = TRUE)
", varCode(newlab), varCode(newlev), datname, dnames1[i], datname, dnames1[i])
        catchange <- TRUE
      }
    } else if(catstate1 == 0) {
      if(modify) {
        # convert to character
        dat[,dnames1[i]] <- as.character(dat[,dnames1[i]])
        # add back attributes
        attr(dat[,dnames1[i]], 'label') <- curattr$label
        attr(dat[,dnames1[i]], 'units') <- curattr$units
      }
      logmsg <- sprintf('# stringifying factor variable [%s]', dnames1[i])
      codemsg <- sprintf("%s[,'%s'] <- as.character(%s[,'%s'])", datname, dnames1[i], datname, dnames1[i])
      codemsg <- c(codemsg, sprintf("attr(%s[,'%s'], 'label') <- '%s'", datname, dnames1[i], curattr$label))
      codemsg <- c(codemsg, sprintf("attr(%s[,'%s'], 'units') <- '%s'", datname, dnames1[i], curattr$units))
    }
    if(catchange) {
      if(modify) {
        attr(dat[,dnames1[i]], 'label') <- curattr$label
        attr(dat[,dnames1[i]], 'units') <- curattr$units
      }
      logmsg <- sprintf('# updating labels in factor variable [%s]', dnames1[i])
      codemsg <- c(codemsg, sprintf("attr(%s[,'%s'], 'label') <- '%s'", datname, dnames1[i], curattr$label))
      codemsg <- c(codemsg, sprintf("attr(%s[,'%s'], 'units') <- '%s'", datname, dnames1[i], curattr$units))
    }
    log <- c(log, logmsg, codemsg)
    if('recode' %in% names(v1)) {
      rcinstr <- v1$recode
      oldvals <- sapply(rcinstr, `[[`, 'oldvalue')
      newvals <- sapply(rcinstr, `[[`, 'newvalue')
      curattr <- attributes(dat[,dnames1[i]])
      if(modify) {
        datm <- newvals[match(dat[,dnames1[i]], oldvals)]
        dat[,dnames1[i]] <- datm
        attributes(dat[,dnames1[i]]) <- curattr
      }
      log <- c(log, sprintf('# recoding variable [%s]', dnames1[i]))
      log <- c(log, sprintf("%s
%s
%s
datm <- newvals[match(%s[,'%s'], oldvals)]
%s[,'%s'] <- datm
attributes(%s[,'%s']) <- curattr
", varCode(oldvals), varCode(newvals), varCode(curattr), datname, dnames1[i], datname, dnames1[i], datname, dnames1[i]))
    }
  }
  cat(paste(log, collapse = '\n'), file = codefile)
  if(modify) {
    dat
  } else {
    invisible(dat)
  }
}
