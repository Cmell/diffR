
diffEst <- function (
  pid,
  dataDir, 
  logDir,
  baseCtl,
  returnResult = F,
  writeResult = T,
  df=NULL,
  varNames=NULL,
  fastDmLoc='./fast-dm',
  stdOutLoc=NULL,
  saveMode='a+',
  ...
) {
  #' Estimates a single diffusion model via fast-dm.
  #'
  #' @param pid The file prefix on the ".dat" file to process.
  #' @param dataDir The directory containing data files to process.
  #' @param logDir The directory to save log files to.
  #' @param baseCtl A character string containing the basic control file without
  #' a load or log statement.
  #' @param returnResult If TRUE (not default), a dataframe with 1 row is 
  #' returned containing the results of the estimation.
  #' @param writeResult If TRUE (default) the result is written to a .lst file
  #' in the \code{logDir}.
  #' @param df Dataframe to process data from. Currently not tested.
  #' @param varNames Character vector of variable names to use from \code{df}.
  #' @param fastDmLoc Path to the fast-dm executable, including the names of the
  #' executable.
  #' @param stdOutLoc A filename to save output to. Default is to not save
  #' output.
  #' @param saveMode 
  #' @param ... Other parameters to pass to \code{system()}. Note that
  #' \code{intern} is already set according to whether \code{stdOutLoc} is 
  #' provided.
  #'
  #' @return If 
  #' @export
  
  # Note that baseCtl should not have a load or log statement. This is added.
  datFl <- paste0(dataDir, '/', pid, '.dat')
  logFl <- paste0(logDir, '/', pid, '.lst')
  ctlFl <- paste0(dataDir, '/', pid, '.ctl')
  
  if (!dir.exists(logDir)) {
    dir.create(logDir)
  }
  
  # Write the data file if no df if provided
  rmDatFl <- F
  if (!is.null(df)) {
    # doesn't work yet
    if (is.null(varNames)) {
      varNames <- colnames(df)
    }
    write.table(df, file=datFl, sep=" ", col.names = F, row.names = F)
    rmDatFl <- T
  }
  
  # Write the control file
  ctl <- paste0(baseCtl, '\nload ', datFl, '\n', 'log ', logFl)
  fileConn <- file(ctlFl)
  writeLines(ctl, fileConn)
  close(fileConn)
  
  # Construct the command
  sysCmd <- paste(fastDmLoc, ctlFl)
  sysResult <- system(sysCmd,
                      intern=(!is.null(stdOutLoc)),
                      ...)
  
  # Save output if file is specified
  if (!is.null(stdOutLoc)) {
    # First, get lock.
    # create lock file if not there.
    lckFile <- paste0(stdOutLoc, '.lock')
    if (!file.exists(lckFile)) {
      file.create(lckFile)
    }
    lck <- filelock::lock(lckFile)
    flConn <- file(stdOutLoc, open=saveMode)
    write(sysResult, file=flConn)
    close(flConn)
    filelock::unlock(lck)
  }
  
  # Clean up
  file.remove(ctlFl)
  
  # Return the result
  if (returnResult) {
    # Read the output
    res <- read.table(logFl, header=T)
    if (!writeResult) {
      file.remove(logFl)
    }
    return(res)
  }
}
