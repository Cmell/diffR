plotCDFValues <- function (..., pars=NULL, plotcdfLoc='./plot-cdf', 
                           outFile='tempCDF.lst', retainOutputFile = F) {
  #' Title
  #'
  #' @param ... Named parameters from the diffusion model, if not given as a 
  #' list in \code{pars}
  #' @param pars Diffusion model parameter set. Should be in a named list.
  #' @param plotcdfLoc The location of the plotcdf program.
  #' @param outFile The temporary file that will hold results of plotcdf.
  #'
  #' @details
  #' The parameters that must be provided are \code{a}, \code{zr}, \code{v},
  #' \code{t0}, \code{d}, \code{szr}, \code{sv}, and \code{st0}. 
  #'
  #' @return A set of values for the predicted CDF.
  #' @export plotCDF

  
  # Extract arguments if necessary.
  if (is.null(pars)) {pars = list(...)}
  parsNeeded <- c('a', 'zr', 'v', 't0', 'd', 'szr', 'sv', 'st0')
  for (i in parsNeeded) {
    if (i %in% names(pars)) {
      assign(i, pars[i])
    } else {
      stop(
        paste0(
          "Must supply all parameter values! Parameter ",
          i, " not found."
        )
      )
    }
  }
  # Construct a command
  cmnd <- c(
    plotcdfLoc,
    '-a', a,
    '-z', zr,
    '-v', v,
    '-t', t0,
    '-d', d,
    '-Z', szr,
    '-V', sv,
    '-T', st0,
    '-o', paste0('"',outFile,'"', sep='')
  )
  print(cmnd <- paste(cmnd, collapse = ' '))
  system(cmnd)
  dtemp <- read.table(outFile)
  if (!retainOutputFile) {unlink(outFile)}
  colnames(dtemp) <- c('rt', 'cd')
  return(dtemp)
}

plotCDF <- function (..., realData = NULL,
                     plotPars = NULL,
                     linesPars = NULL) {
  #' Title
  #'
  #' @param ... Parameters for \code{plotCDFValues}
  #' @param realData A data frame or with 2 colums: the first should be an 
  #' \code{rt} column and the second should be a \code{response} column that 
  #' has a 0 for each lower boundary response and a 1 for upper boundardy
  #' responses.
  #' @param plotPars A named list of arguments to pass to the \code{plot()}
  #' call.
  #' @param linesPars A named list of arguments to pass to the \code{lines()}
  #' call that plots the \code{realData} cdf (if provided).
  #'
  #' @export plotCDF
  
  vals <- plotCDFValues(...)
  if (!is.null(plotPars)) {
    plotPars <- c(list(x=vals$rt, y=vals$cd), plotPars)
  } else {
    # defaults
    plotPars <- list(x=vals$rt, y=vals$cd, type = 'l')
  }
  # It tries to use the whole set of values for labels, so overwrite if not
  #provided
  if (!('xlab' %in% names(plotPars))) {
    plotPars$xlab = 'rt'
  }
  if (!('ylab' %in% names(plotPars))) {
    plotPars$ylab = 'cumulative density'
  }
  do.call(plot, plotPars)
  #plot(x=vals$rt, y=vals$cd)
  
  # If real data are provided, add some lines with them.
  if (!is.null(realData)) {
    realData[,1] <- ifelse(
      realData[,2] == min(realData[,2]),
      -1 * (realData[,1]), 
      realData[,1]
    )
    realCdfFn <- ecdf(realData[,1])
    # generate values from the x's returned by plot cdf
    realCd <- realCdfFn(vals$rt)
    if (!is.null(plotPars)) {
      linesPars <- c(list(x=vals$rt, y=realCd), linesPars)
    } else {
      linesPars <- list(x=vals$rt, y=realCd)
    }
    do.call(lines, linesPars)
  }
}