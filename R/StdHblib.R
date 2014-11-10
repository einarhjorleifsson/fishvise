#attach("/home/einarhj/stasi/StdHblib/.RData")

#' @title Rattach
#' 
#' @description Modified \code{attach}. If in search path is, unless otherwise
#' specied put in search path 2.
#' 
#' @export
#' 
#' @param what The directory containing .RData
#' @param pos integer specifying position in search() where to attach.
#'            If not specified, will attached to search path 2.
#' @param ... other arguments to pass to \code{attach}
Rattach <- function (what, pos, ...) 
{
  if (file_test("-d", what)) 
    what <- paste(what, ".RData", sep = "/")
  txt <- paste("file:", what, sep = "")
  if (!is.na(match(txt, search()))) {
    pos1 <- match(txt, search())
    detach(pos = pos1)
    if (missing(pos)) 
      attach(what, pos = pos1, ...)
    else attach(what, pos = pos, ...)
  }
  else {
    attach(what, ...)
  }
}

#' @title residplot
#' 
#' @description A bubble plot of residuals
#' 
#' @export
#' 
#' @author Hoskuldur Bjornsson
#' 
#' @param residuals XXX
#' @param xpos XXX
#' @param ypos XXX
#' @param maxsize XXX
#' @param poscol XXX
#' @param linecol XXX
#' @param lwd XXX
#' @param n XXX
#' @param maxn XXX
#' @param negcol XXX
#' @param txt XXX
#' @param cex XXX
#' @param xlab XXX
#' @param ylab XXX
#' @param axes XXX
#' @param arg XXX
#' @param argcol XXX
#' @param arglty XXX
#' @param cn XXX
#' @param append XXX
#' 
residplot <- function (residuals, xpos, ypos, maxsize = 0.2, poscol = 2, linecol = 1, 
          lwd = 1, n = 50, maxn, negcol, txt = F, cex = 0.5, xlab = "", 
          ylab = "", axes = T, arg = T, argcol = 20, arglty = 2, cn = c("x", 
                                                                        "y", "z"), append = F) 
{
  par(err = -1)
  if (is.data.frame(residuals)) {
    x <- residuals[, cn[1]]
    y <- residuals[, cn[2]]
    residuals <- residuals[, cn[3]]
  }
  else {
    residuals <- t(residuals)
    if (missing(maxn)) 
      maxn <- max(abs(residuals), na.rm = T)
    if (missing(xpos)) 
      xpos <- 1:nrow(residuals)
    if (missing(ypos)) 
      ypos <- 1:ncol(residuals)
    x <- matrix(xpos, length(xpos), length(ypos))
    y <- matrix(ypos, length(xpos), length(ypos), byrow = T)
  }
  if (!append) 
    plot(x, y, type = "n", xlab = xlab, ylab = ylab, axes = axes)
  x.bck <- x
  y.bck <- y
  if (arg) {
    r <- x.bck - y.bck
    tmp <- unique(r)
    for (i in 1:length(tmp)) {
      j <- r == tmp[i]
      lines(x.bck[j], y.bck[j], col = argcol, lty = arglty)
    }
  }
  plt <- par()$pin
  xscale <- (par()$usr[2] - par()$usr[1])/plt[1] * maxsize
  yscale <- (par()$usr[4] - par()$usr[3])/plt[2] * maxsize
  rx <- c(unlist(sqrt(abs(residuals)/maxn) * xscale))
  ry <- c(unlist(sqrt(abs(residuals)/maxn) * yscale))
  theta <- seq(0, 2 * pi, length = n)
  n1 <- length(rx)
  theta <- matrix(theta, n1, n, byrow = T)
  x <- matrix(x, n1, n)
  y <- matrix(y, n1, n)
  rx <- matrix(rx, n1, n)
  ry <- matrix(ry, n1, n)
  x <- x + rx * cos(theta)
  y <- y + ry * sin(theta)
  x <- cbind(x, rep(NA, nrow(x)))
  y <- cbind(y, rep(NA, nrow(y)))
  i <- residuals > 0
  if (any(i)) {
    polygon(c(t(x[i, ])), c(t(y[i, ])), col = poscol)
    lines(c(t(x[i, ])), c(t(y[i, ])), col = linecol, lwd = lwd)
  }
  i <- residuals < 0
  if (any(i)) {
    if (!missing(negcol)) 
      polygon(c(t(x[i, ])), c(t(y[i, ])), col = negcol)
    lines(c(t(x[i, ])), c(t(y[i, ])), col = linecol, lwd = lwd)
  }
  if (txt) 
    text(x.bck, y.bck, as.character(round(residuals)), cex = cex)
  return(invisible())
}


#' @title ypr
#' 
#' @description A yield per recruit model
#' 
#' @export
#' 
#' @param fmort A vector containing the fishing mortality over which to run the ypr
#' @param cW Catch weights
#' @param selC Selection pattern of the catch
#' @param ages A vector containing the age classes
#' @param plusgroup Boolean If FALSE (default) no plusgroup bookkeeping
#' @param a1 A value containing lower reference age
#' @param a2 A value containing upper reference age
#' @param dW Discard weights
#' @param ssbW Spawning stock weights
#' @param selL Selection pattern of the landings
#' @param M Natural mortality, can be a single value
#' @param mat A vector of proportion mature
#' @param pM Partial natural mortality prior to spawning
#' @param pF Partial fishing mortality prior to spawning
#' @param ssb A boolean (Not used)
#' 
ypr <- function(fmort, cW, selC, ages=3:14,plusgroup=F,a1=5,a2=10, dW, ssbW, selL, M,mat,pM=0,pF=0,ssb=F){
  
  if(missing(cW)) stop("Catch weights need to be specified")
  if(missing(selC))   stop("Selection at age for catches must be specified")
  if(missing(ages)) stop("Age vector must be specified")
  
  if(missing(dW))    dW <- rep(0,length(ages))
  if(missing(ssbW))  ssbW <- cW
  
  if(missing(mat))    mat <- rep(1,length(ages)) 
  if(length(M) == 1)  M <- rep(M,length(ages))
  if(length(pM) == 1) pM <- rep(pM,length(ages))
  if(length(pF) == 1) pF <- rep(pF,length(ages))
  
  if(missing(selL))   selL <- selC
  
  age <- a1:a2
  i <- !is.na(match(ages,age))
  
  selL <- selL/mean(selC[i]) 
  selC <- selC/mean(selC[i]) 
  
  res <- data.frame(fmort=fmort,yield=rep(0,length(fmort)))
  res$SSB <- res$discardN <-res$discardWt <- res$yield
  for(i in 1:nrow(res)) {
    tmp <- ypr_single_target(res$fmort[i], cW, selC,selL,M,ages=ages,plusgroup=plusgroup,mat=mat,pM=pM,pF=pF,dW=dW,ssbW=ssbW)
    res$yield[i] <- tmp$Catch
    res$SSB[i] <- tmp$SSB
    res$discardN[i] <- tmp$Discard
    res$discardWt[i] <- tmp$Discardwt
    
  }
  x <- spline(res$fmort,res$yield,n=10*nrow(res))
  maxy <- max(x$y)
  fmax <- x$x[x$y==max(x$y)]
  d <- diff(x$y)
  d <- d/d[1]
  d1 <- d[1:(length(d)-1)]
  d2 <- d[2:length(d)]
  i <- 1:length(d1)
  i <- i[d1 > 0.1 & d2 < 0.1]
  f01 <- x$x[i]
  
  x <- spline(res$fmort,res$SSB,n=10*nrow(res))
  x1  <- abs(x$y-0.35*max(res$SSB))
  fssb35 <- x$x[x1==min(x1)]
  ssb35 <- x$y[x1==min(x1)]
  refs <- data.frame(f01=f01,fmax=fmax[1],fssb35=fssb35,ssb35=ssb35,maxy=maxy)
  #referencept <- rep(0,4)
  #names(referencept) <- c("f01","fmax","maxy","ssb35")
  #referencept["f01"] <- f01
  #referencept["fmax"] <- fmax[1]
  #referencept["maxy"] <- maxy
  #referencept["ssb35"] <- ssb35
  
  #attributes(res)$refpt <- referencept
  return(list(res=res,refs=refs))
}

#' @title ypr
#' 
#' @description A yield per recruit model
#' 
#' @export
#' 
#' @param fmort A vector containing the fishing mortality over which to run the ypr
#' @param cW Catch weights
#' @param selC Selection pattern of the catch
#' @param selL Selection pattern of the landings
#' @param M Natural mortality, can be a single value
#' @param ages A vector containing the age classes
#' @param plusgroup Boolean If FALSE (default) no plusgroup bookkeeping
#' @param mat A vector of proportion mature
#' @param pM Partial natural mortality prior to spawning
#' @param pF Partial fishing mortality prior to spawning
#' @param dW Discard weights
#' @param ssbW Spawning stock weights
#' 
ypr_single_target <- function(fmort, cW, selC,selL,M,ages=c(2:9),plusgroup=plusgroup,mat,pM=0,pF=0,dW,ssbW)
{
  if(missing(mat)) mat <- rep(1,length(ages)) 
  if( missing(ssbW)) ssbW <- cW
  if(length(M) == 1) M <- rep(M,length(ages))
  if(length(pM) == 1) pM <- rep(pM,length(ages))
  if(length(pF) == 1) pF <- rep(pF,length(ages))
  
  Fishmort <- selC * fmort
  Fishmortlan <- selL * fmort
  Fishmortdisc <-  Fishmort-Fishmortlan
  Z <- Fishmort + M
  N <- 1000000.
  Catch <- SSB <-Discard <- Discardwt <-   0
  for(i in 1:length(ages)) {
    SSB <- SSB + N*exp(-(Fishmort[i]*pF[i]+M[i]*pM[i]))*ssbW[i]*mat[i]
    Catch <- Catch + Fishmortlan[i]/Z[i] * N * (1 - exp( - Z[i])) *
      cW[i]
    Discard <- Discard +  Fishmortdisc[i]/Z[i] * N * (1 - exp( - Z[i])) # Numbers
    Discardwt <- Discardwt +  Fishmortdisc[i]/Z[i] * N * (1 - exp( - Z[i]))*dW[i] # Biomass   
    N <- N * exp( - Z[i])
  }
  if(plusgroup) { # 10 umfer?ir enn ekki brottkast ? plusgruppu
    n <- length(ages)
    for(i in 1:10) {
      Catch <- Catch + Fishmortlan[n]/Z[n] * N * (1 - exp( - Z[n])) *  # ??ur var sm? villa h?r cW[i]
        cW[n]
      SSB <- SSB + N*exp(-(Fishmort[n]*pF[n]+M[n]*pM[n]))*ssbW[n]*mat[n]
      
      N <- N * exp( - Z[n])
    }
  }
  return(list(Catch=Catch/1e6,SSB=SSB/1e6,Discard=Discard/1e6,Discardwt=Discardwt/1e6))
}