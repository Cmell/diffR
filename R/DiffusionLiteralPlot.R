
diffPlotHelper <- function(pnum, responseVar, rtVar, rawData, diffResults,
                           condition=c("con_b", "con_s", "con_d", "con_w",
                                       "incon_b", "incon_s", "incon_d", "incon_w"), ...){
  
  condition <- match.arg(condition)
  conditionalPars <- c("zr", "a", "v", "t0", "d")
  unconditionalPars <- c("szr", "sv", "st0")
  condVec <- strsplit(condition, split="_")
  
  condParsC <- paste(conditionalPars, condition, sep="_")
  
  tRT <- rawData[rawData$Subnum==pnum & 
                   rawData[,responseVar]==1 & 
                   rawData$blockType==condVec[[1]][1] & 
                   rawData$stim==condVec[[1]][2], 
                 rtVar]
  bRT <- rawData[rawData$Subnum==pnum & 
                   rawData[,responseVar]==0 & 
                   rawData$blockType==condVec[[1]][1] & 
                   rawData$stim==condVec[[1]][2], 
                 rtVar]
  #bRT <- filter_(rawData, responseVar==0, "Subnum"==pnum) %>% select_(rtVar)
  
  parVec <- filter(diffResults, dataset==pnum)[,c(condParsC, unconditionalPars)]
  
  #print(parVec)
  
  diffPlot(v=parVec, topRTs=tRT, bottomRTs=bRT, ...)
}

diffPlot <- function(v, z, t0, a, zr,
                     sv, szr, st0,
                     ymin=NULL, ymax=NULL,
                     tmin=NULL, tmax=NULL, tExtFactor=1.2,
                     topRTs=NULL, bottomRTs=NULL,
                     topLab=NULL, bottomLab=NULL,
                     aLty=2, aLwd=1, aCol="gray",
                     vLty=1, vLwd=2, vCol="blue", vLength=0.20,
                     vAngle=20, vCode=2,
                     t0Lty=1, t0Lwd=1, t0Col="magenta", t0Length=0.10,
                     t0Angle=90, t0Code=3,
                     xlab="time (seconds)", ylab="evidence",
                     main="", tLimit=2.5){
  #' Plotting the Diffusion Model
  #' 
  #' This function constructs a diffusion model plot. It shows average parameter
  #' values.
  #'
  #' @param v The drift rate.
  #' @param z The starting point. Note that it is in the metric with a range 0 to
  #' \code{a}. Either this or \code{zr} must be given.
  #' @param t0 Nondecision time.
  #' @param a Threshold value.
  #' @param zr Relative starting point. Used if \code{z} is missing, and should
  #' represent a proportion of the \code{a} distance.
  #' @param sv Variability of the drift rate.
  #' @param szr Variability of the relative starting point.
  #' @param st0 Variability of nondecision time.
  #' @param ymin Y (evidence) axis minimum value.
  #' @param ymax Y (evidence) axis maximum value.
  #' @param tmin Minimum time value to show.
  #' @param tmax Maximum time value to show.
  #' @param tExtFactor 
  #' @param topRTs 
  #' @param bottomRTs 
  #' @param topLab 
  #' @param bottomLab 
  #' @param aLty 
  #' @param aLwd 
  #' @param aCol 
  #' @param vLty 
  #' @param vLwd 
  #' @param vCol 
  #' @param vLength 
  #' @param vAngle 
  #' @param vCode 
  #' @param t0Lty 
  #' @param t0Lwd 
  #' @param t0Col 
  #' @param t0Length 
  #' @param t0Angle 
  #' @param t0Code 
  #' @param xlab 
  #' @param ylab 
  #' @param main 
  #' @param tLimit 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  
  if(!require(beeswarm)){
    install.packages("beeswarm")
    library(beeswarm)
  }
  
  if(length(v)>1){
    vec <- v
    for(i in 1:length(vec)){
      c <- strsplit(names(vec)[i], "_")
      assign(c[[1]][1], as.numeric(vec[i]))
    }
  }
  print(t0)
  if(missing(z)) {z=zr*a}
  print("43")
  if(missing(zr)) {zr=z/a}
  print("45")
  # set up plot dimensions
  if(v>0) vEndT <- ((a-z)/v) + t0
  if(v<0) vEndT <- ((0-z)/v) + t0
  if(v==0) vEndT <- tLimit
  
  vStartX <- t0
  vEndY <- v * (vEndT-t0) + z
  
  if(is.null(ymin)) ymin <- 0
  if(is.null(ymax)) ymax <- 1.1 * a
  
  if(is.null(tmin)) tmin <- 0
  
  # tmax depends on wether or not there are response times given
  if(!(is.null(topRTs) | is.null(bottomRTs))){
    if(is.null(tmax)){
      tmax <- max(c(topRTs, bottomRTs))
    }
    plotDensities <- TRUE
  } else {
    if(is.null(tmax)){
      tmax <- vEntT * tExtFactor
    }
  }
  
  # Get the layout set up and graphic parameters acceptable:
  layMat <- matrix(c(2, 1, 3), nrow=3, ncol=1)
  hts <- c(.2, .6, .2)
  layout(layMat, heights=hts)
  op <- par(mar=c(2, 4, 0, 1))
  
  # Basic plot
  plot(x=NA, y=NA, xlim=c(tmin, tmax), ylim=c(ymin, ymax),
       xlab=xlab, ylab=ylab, main=main, bty="l")
  
  # add a
  lines(x=c(0, tmax), y=c(a, a), lty=aLty, lwd=aLwd, col=aCol)
  # add v
  arrows(x0=vStartX, y0=z, x1=vEndT, y1=vEndY,
         lty=vLty, lwd=vLwd, col=vCol, length=vLength,
         angle=vAngle, code=vCode)
  # add t0
  arrows(x0=0, y0=z, x1=t0, y1=z,
         lty=t0Lty, lwd=t0Lwd, col=t0Col, length=t0Length,
         angle=t0Angle, code=t0Code)
  # add boundary labels
  if(!is.null(topLab)){
    text(x=((vEndT + tmax)/2), y=(a-.1),
         labels=topLab)
  }
  if(!is.null(bottomLab)){
    text(x=((vEndT + tmax)/2), y=(.1),
         labels=bottomLab)
  }
  
  if(plotDensities){
    # make a plot that includes the density lines.
    if(length(topRTs)>1){
      topDens <- density(topRTs)
      plot(x=NA, y=NA, xlim=c(tmin,tmax), ylim=c(0, max(topDens$y)),
           axes=F, xlab="", ylab="")
      lines(topDens)
      beeswarm(x=topRTs, at=.1, vertical=F, side=1, add=T)
    } else{
      plot(x=NA, y=NA, xlim=c(tmin,tmax), ylim=c(0, 2),
           axes=F, xlab="", ylab="")
      beeswarm(x=topRTs, at=.1, vertical=F, side=1, add=T)
    }
    
    if(length(bottomRTs)>1){
      bottomDens <- density(bottomRTs)
      plot(x=NA, y=NA, xlim=c(tmin,tmax), ylim=c(min(-bottomDens$y), 0),
           axes=F, xlab="", ylab="")
      beeswarm(x=bottomRTs, at=-.1, vertical=F, side=-1, add=T,
               method="square")
      lines(x=bottomDens$x, y=-bottomDens$y)
    } else{
      plot(x=NA, y=NA, xlim=c(tmin,tmax), ylim=c(-4, 0),
           axes=F, xlab="", ylab="")
      beeswarm(x=bottomRTs, at=-.1, vertical=F, side=-1, add=T,
               method="square")
    }
  }
  
  par(op)
}

#diffPlot(v=.3, z=.5, a=1, t0=.1)
