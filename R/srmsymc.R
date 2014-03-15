#' @title Read key parameter estimates from the srmsymc program
#' 
#' @description Reads the fiel \code{simpar.dat} generate by the \code{srmsymc}.
#' Returns a list containing the stochastic and the deterministic estimates.
#' 
#' @param file the file name, should not be anything else than \code{simpar.dat}
#' @param path character containing the path to the file. If missing it is
#' assumed that the file is in the current working directory.
#' @param srno integer. 1=Ricker, 2=Beverton-Holt, 3=Hockey-stick.
#' @param minSSB numerical. Minimum observed SSB value. Used for the hockey-stick,
#' excluding values below the lowest observed. If set to 0 (default) do not exclude
#' values below the minimum.
#' @param trim boolean. If TRUE (default) come cleaning (NEED TO SPECIFY) is done.
#' @param longformat boolean. If TRUE (default) return a long format.
#' @export

read.mc.par <- function(file="simpar.dat",path,srno=1,minSSB=0,trim=TRUE,longformat=TRUE) {
  
  if(!missing(path)) file <- paste(path, "/", file, sep="")
  if (!file.exists(file)) stop(paste(file, "not found"))
  
  d = read.table(file)
  names(d) = c("iter","ap","bp","alpha","beta","sigr","scor",
               "fcrash","fmax","f01","f20","f25","f30","f35","f40","fmsy",
               "msy","bmsy","msypr","bmsypr","msyr",
               "fnfcrash","fnfmax","fnf01","fnf20","fnf15",
               "fnf30","fnf35","fnf40","dydf","penalty","nll","AIC")
  
  d.det <- subset(d, iter == 0)
  d.det$iter <- 0
  
  d <- subset(d, iter != 0)
  d$iter <- c(1:nrow(d))
  # get rid of negative/unresonable parameters
  if(trim) {
    #  
    if (srno==3) {
      i <- d$beta < minSSB | d$alpha <= 0 | d$beta <= 0
    } else {
      i <- d$alpha <= 0 | d$beta <= 0
    }
    cn <- c("ap","bp","alpha","beta","sigr","scor","fcrash","fmsy",
            "msy","bmsy","fnfcrash","dydf","penalty","nll","AIC")
    d[i,cn] <- NA
    
    # Make msy-points as NA if fmsy is greater than 3 
    if (any(!d$fmsy < 3 , na.rm=TRUE)) d[which(!d$fmsy < 3),c("fmsy","bmsy","msy")] <- NA
    # Make Fcrash-points as NA if above 5
    if (any(!d$fcrash<5, na.rm=TRUE)) d[which(!d$fcrash<5),"fcrash"] <- NA
    # Similar for Fmas greater than 3
    if (any(!d$fmax<3 , na.rm=TRUE)) d[which(!d$fmax<3),c("fmax","bmsypr","msypr")] <- NA
    # columns of outputpr[[srtype]] to be output to output[[srtype]]
    indexmap = c("f20","f25","f30","f35","f40","f01") 
    for (i in indexmap) if (any(!d[,i] < 3, na.rm=TRUE)) d[which(!d[,i]<3),i] <- NA
  }
  
  if(longformat) {
    d <- melt(d,id.vars='iter')
    d.det <- melt(d.det,id.vars='iter')
  }
  
  return(list(stochastic=d,
              deterministic=d.det))
}

#' @title Read the yield estimates the srmsymc program
#' 
#' @description Reads the file \code{simpary.dat} generate by the \code{srmsymc}.
#' In the \code{simpary.dat} the first row is the fishing mortality,
#' the second row is the deterministic estimates of yield and the remaining
#' rows represents the catch for each iterations.
#' Returns a list containing two data.frames, one containing the deterministic
#' estimate, the other containing the stochastic simulations.
#' 
#' @param file the file name, should not be anything else than \code{simpary.dat}
#' @param path character containing the path to the file. If missing it is
#' assumed that the file is in the current working directory.
#' @param srno integer. 1=Ricker, 2=Beverton-Holt, 3='Hockey-stick.
#' @param trim boolean. If TRUE (default) come cleaning (NEED TO SPECIFY) is done.
#' @param longformat boolean. If TRUE (default) return a long format.
#' @param doPlot boolean. If TRUE (default) returns a ggplot
#' @export

read.mc.yield <- function(file="simpary.dat",path,srno=1,trim=TRUE,longformat=TRUE,doPlot=TRUE) {
  
  if(!missing(path)) file <- paste(path, "/", file, sep="")
  if (!file.exists(file)) stop(paste(file, "not found"))
  
  d = as.matrix(read.table(file))
  
  if(trim) {
    d[-1,] = pmax(d[-1,],0)
    noredlines = prod(dim(d)==0)
  }
  
  colNames <- d[1,]
  d <- as.data.frame(d[-1,])
  names(d) <- colNames
  d.det <- d[1,]
  d.det$iter <- 0
  
  d <- d[-1,]
  d$iter <- c(1:nrow(d))
  
  if(longformat) {
    d <- melt(d,id.vars='iter')
    d$variable <- as.numeric(as.character(d$variable))
    d.det <- melt(d.det,id.vars='iter')
    d.det$variable <- as.numeric(as.character(d.det$variable))
    
    x <- calc.quantiles(d,d.det=d.det)
    
    if(doPlot) {
      gg.plot <- do.ggplot(x,d)
      return(list(deterministic = d.det,
                  stochastic = d,
                  quantiles = x,
                  gg.plot = gg.plot))
    } else {
      return(list(deterministic = d.det,
                  stochastic = d,
                  quantiles = x))
    }
  }
  
  if(!longformat) stop('Not yet implemented for wide format')
  
}


#' @title Read the ssb estimates the srmsymc program
#' 
#' @description Reads the file \code{simpary.dat} generate by the \code{srmsymc}.
#' In the \code{simpary.dat} the first row is the fishing mortality,
#' the second row is the deterministic estimates of ssb and the remaining
#' rows represents the catch for each iterations.
#' Returns a list containing two data.frames, one containing the deterministic
#' estimate, the other containing the stochastic simulations.
#' 
#' @param file the file name, should not be anything else than \code{simpary.dat}
#' @param path character containing the path to the file. If missing it is
#' assumed that the file is in the current working directory.
#' @param srno integer. 1=Ricker, 2=Beverton-Holt, 3='Hockey-stick.
#' @param trim boolean. If TRUE (default) come cleaning (NEED TO SPECIFY) is done.
#' @param longformat boolean. If TRUE (default) return a long format.
#' @param doPlot boolean. If TRUE (default) returns a ggplot
#' @export

read.mc.ssb <- function(file="simparssb.dat",path,srno=1,trim=TRUE,longformat=TRUE,doPlot=TRUE) {
  
  if(!missing(path)) file <- paste(path, "/", file, sep="")
  if (!file.exists(file)) stop(paste(file, "not found"))
  
  d = as.matrix(read.table(file))
  
  if(trim) {
    d[-1,] = pmax(d[-1,],0)
    noredlines = prod(dim(d)==0)
  }
  
  colNames <- d[1,]
  d <- as.data.frame(d[-1,])
  names(d) <- colNames
  d.det <- d[1,]
  d.det$iter <- 0
  
  d <- d[-1,]
  d$iter <- c(1:nrow(d))
  
  if(longformat) {
    d <- melt(d,id.vars='iter')
    d$variable <- as.numeric(as.character(d$variable))
    d.det <- melt(d.det,id.vars='iter')
    d.det$variable <- as.numeric(as.character(d.det$variable))
    
    x <- calc.quantiles(d,d.det=d.det)
    
    if(doPlot) {
      gg.plot <- do.ggplot(x,d)
      return(list(deterministic = d.det,
                  stochastic = d,
                  quantiles = x,
                  gg.plot = gg.plot))
    } else {
      return(list(deterministic = d.det,
                  stochastic = d,
                  quantiles = x))
    }
  }
  
  if(!longformat) stop('Not yet implemented for wide format')
}

#' @title Calculate recruitment
#' 
#' @description xxx
#' 
#' @param ssb x
#' @param alpha a
#' @param beta b
#' @param sr sr-type (numerical)
#' @export
calc.ssb.r <- function(ssb,alpha,beta,sr) {
  q05 <- q10 <- q16 <- q50 <- q84 <- q90 <- q95 <- value <- variable <- NULL
  niter <- length(alpha)
  d = matrix(NA,niter,length(ssb))
  for (i in 1:niter) d[i,] = recruitment(ssb, alpha[i], beta[i], sr)
  d <- as.data.frame(d)
  d$iter <- 1:niter
  d <- melt(d,id.vars='iter',value.name='value')
  d$variable <- rep(ssb,each=niter)
  
  #x <- ddply(d,c("variable"),summarise,
  #           q05=quantile(value,0.05),
  #           q10=quantile(value,0.10),
  #           q16=quantile(value,0.16),
  #           q50=quantile(value,0.50),
  #           q84=quantile(value,0.84),
  #           q90=quantile(value,0.90),
  #           q95=quantile(value,0.95))
  #x$mean <- d.det$value
  x <- calc.quantiles(d)
  
  gg.plot <- ggplot(x,aes(variable)) +  
    geom_ribbon(aes(ymin=q05,ymax=q95),fill='grey',alpha=1/2) +
    geom_ribbon(aes(ymin=q10,ymax=q90),fill='grey',alpha=1/2) +
    geom_ribbon(aes(ymin=q16,ymax=q84),fill='grey',alpha=1/2) +
    geom_line(data=d,aes(variable,value,group=iter),alpha = 0.05,col='red') +
    geom_line(aes(y=q50),col='red',lwd=1) #+
  #geom_line(aes(y=mean),col='blue',lwd=1)
  
  return(list(calculations = d,
              quantiles = x,
              gg.plot = gg.plot))
}

#' @title Make this an internal function
#' 
#' @param SSB xxx
#' @param alpha xxx
#' @param beta xxx
#' @param sr xxx
#' @export
recruitment = function(SSB, alpha, beta, sr)
{
  if (sr == 1)
  {
    #Ricker
    recruitment = alpha * SSB*exp(-beta * SSB)
  } else if (sr == 2) {
    #Beverton Holt
    recruitment = alpha * SSB /(beta + SSB)
  } else {
    #Smooth Hockeystick
    g = 0.001
    recruitment = alpha*(SSB+sqrt(beta^2+g)-sqrt((SSB-beta)^2+g))
    
  }
  return(recruitment)
}

#' @title Run srmsymc
#' 
#' @description Run the ADM srmsymc program
#' 
#' @author Einar Hjorleifsson <einar.hjorleifsson@gmail.com>
#' @param sr integer. containg the stock-recruitment model
#' @param opt_sen XXX
#' @param opt_pen XXX
#' @param nits Number of iterations of bootstrapping - if 0, does only the deterministic fit
#' @param path characters. Name of the directory for storing the output. If
#' missing, the output remains in the working directory.
#' @param output boolean. If FALSE (default) no results (*.dat) files are returned.

#' @export

run.srmsymc <- function(sr,opt_sen=1,opt_pen=1,nits=100,path,output=FALSE) {
  
  # srmsymc and srmsymc2 does not exists
  if (!file.exists('srmsymc')) { #stop('The srmsymc program must be in the working directory')
    cmd <- paste('cp',paste(path.package("msy"),'extdata/srmsymc.tpl',sep='/'),'.')
    system(cmd)
    system('admb srmsymc')
    cmd <- paste('cp',paste(path.package("msy"),'extdata/srmsymc2.tpl',sep='/'),'.')
    system(cmd)
    system('admb srmsymc2')
  }
  
  if (!file.exists('srmsymc.dat')) stop('The srmsymc.dat file must be in the working directory')
  
  # Update the recruitment number
  x <- readLines("srmsymc.dat")
  i <- grep("# Ropt:   S-R function type",x)
  x[i] <- paste(sr,"  # Ropt:   S-R function type")

  
  i <- grep("# senopt", x)
  x[i] <- paste(opt_sen,"# senopt")
  i <- grep("# penopt", x)
  x[i] <- paste(opt_pen,"# penopt")
  
  write(x,file="srmsymc.dat")
  
  # get the name of the age data file
  #if (!file.exists('out.dat')) stop('The out.dat file must be in the working directory')
  
  tmpfile = file("sim.dat","w")
  cat('21 1 ', nits, "\n", sep="", file = tmpfile)
  close(tmpfile)
  
  system(paste('srmsymc -mcmc', (nits+11)*10000, '-mcsave 10000 -nosdmcmc'))
  system("srmsymc -mceval")
  
  if(!missing(path)) {
    
    if (!file.exists(path)) {
      cmd <- paste("mkdir",path)
      system(cmd)
    }
    
    cmd <- paste("cp *.dat ",path,"/.",sep="")
    system(cmd)
  }
  
  if(output) {
    x <- list()
    x$par <- read.mc.par()
    x$yield <- read.mc.yield()
    x$ssb <- read.mc.ssb()
    return(x)
  }
}

#' @title Read all output files from srmsymc program
#' 
#' @description xxx
#' 
#' @param path x
#' @param ... addition arguements
#' @export
read.srmsymc <- function(path, ...) {
  x <- list()
  x$par   <- read.mc.par(path=path)
  x$yield <- read.mc.yield(path=path)
  x$ssb   <- read.mc.ssb(path=path)
  return(x)
}

#' @title Compile the \code{srmsymc} in the current working directory
#' 
#' @description xx
#' 
#' @param clean boolean. If TRUE (default) removes files generated by \code{admb}
#' when installing.
#' @export
compile.srmsymc <- function(clean=TRUE) {

  file2copy <- system.file("extdata","srmsymc.tpl",package="fishvise")
  cmd <- paste("cp", file2copy, ".")
  system(cmd)
  file2copy <- system.file("extdata","srmsymc2.tpl",package="fishvise")
  cmd <- paste("cp", file2copy, ".")
  system(cmd)
  system("admb srmsymc")
  system("admb srmsymc2")
  if(clean) {
    system("rm *.cpp")
    system("rm *.htp")
  }
  message("srmsymc has been compiled")
}

#' @title Read the ssb per recruit estimates the srmsymc program
#' 
#' @description Reads the file \code{simparssbpr.dat} generate by the \code{srmsymc}.
#' In the \code{simparssbpr.dat} the first row is the fishing mortality,
#' the second row is the deterministic estimates of ssb and the remaining
#' rows represents the catch for each iterations.
#' Returns a list containing two data.frames, one containing the deterministic
#' estimate, the other containing the stochastic simulations.
#' 
#' @param file the file name, should not be anything else than \code{simparssbpr.dat}
#' @param trim boolean. If TRUE (default) come cleaning (NEED TO SPECIFY) is done.
#' @param longformat boolean. If TRUE (default) return a long format.
#' @param doPlot boolean. If TRUE (default) returns a ggplot
#' @export

read.mc.ssbpr <- function(file="simparssbpr.dat",trim=TRUE,longformat=TRUE,doPlot=TRUE) {
  
  if (!file.exists(file)) stop(paste(file, "not found"))
  
  d = as.matrix(read.table(file))
  
  if(trim) {
    d[-1,] = pmax(d[-1,],0)
    noredlines = prod(dim(d)==0)
  }
  
  colNames <- d[1,]
  d <- as.data.frame(d[-1,])
  names(d) <- colNames
  d.det <- d[1,]
  d.det$iter <- 0
  
  d <- d[-1,]
  d$iter <- c(1:nrow(d))
  
  if(longformat) {
    d <- melt(d,id.vars='iter')
    d$variable <- as.numeric(as.character(d$variable))
    d.det <- melt(d.det,id.vars='iter')
    d.det$variable <- as.numeric(as.character(d.det$variable))
    
    x <- calc.quantiles(d,d.det=d.det)
    
    if(doPlot) {
      gg.plot <- do.ggplot(x,d)
      return(list(deterministic = d.det,
                  stochastic = d,
                  quantiles = x,
                  gg.plot = gg.plot))
    } else {
      return(list(deterministic = d.det,
                  stochastic = d,
                  quantiles = x))
    }
  }
  
  if(!longformat) stop('Not yet implemented for wide format')
}

#' @title Read the yield per recruit estimates the srmsymc program
#' 
#' @description Reads the file \code{simparypr.dat} generate by the \code{srmsymc}.
#' In the \code{simparypr.dat} the first row is the fishing mortality,
#' the second row is the deterministic estimates of ssb and the remaining
#' rows represents the catch for each iterations.
#' Returns a list containing two data.frames, one containing the deterministic
#' estimate, the other containing the stochastic simulations.
#' 
#' @param file the file name, should not be anything else than \code{simparypr.dat}
#' @param trim boolean. If TRUE (default) come cleaning (NEED TO SPECIFY) is done.
#' @param longformat boolean. If TRUE (default) return a long format.
#' @param doPlot boolean. If TRUE (default) returns a ggplot
#' @export

read.mc.ypr <- function(file="simparypr.dat",trim=TRUE,longformat=TRUE,doPlot=TRUE) {
  
  if (!file.exists(file)) stop(paste(file, "not found"))
  
  d = as.matrix(read.table(file))
  
  if(trim) {
    d[-1,] = pmax(d[-1,],0)
    noredlines = prod(dim(d)==0)
  }
  
  colNames <- d[1,]
  d <- as.data.frame(d[-1,])
  names(d) <- colNames
  d.det <- d[1,]
  d.det$iter <- 0
  
  d <- d[-1,]
  d$iter <- c(1:nrow(d))
  
  if(longformat) {
    d <- melt(d,id.vars='iter')
    d$variable <- as.numeric(as.character(d$variable))
    d.det <- melt(d.det,id.vars='iter')
    d.det$variable <- as.numeric(as.character(d.det$variable))
    
    x <- calc.quantiles(d,d.det=d.det)
    
    if(doPlot) {
      gg.plot <- do.ggplot(x,d)
      return(list(deterministic = d.det,
                  stochastic = d,
                  quantiles = x,
                  gg.plot = gg.plot))
    } else {
      return(list(deterministic = d.det,
                  stochastic = d,
                  quantiles = x))
    }
  }
  
  if(!longformat) stop('Not yet implemented for wide format')
}

#' @title write data files to disk for the ADM srmsymc program
#' 
#' @description Some details, including link to the function a) running the
#' program and creating the data input
#' 
#' @author Einar Hjorleifsson <einar.hjorleifsson@gmail.com>
#' 
#' @export
#' 
#' @param rby A list that contains in its first three rows year, recruitment
#' and ssb.
#' @param iba A list - read.sen
#' @param aR \emph{integer}. Age of recruitment. Has to be specified. Is used to
#' shift the recruitment data such that it aligns with ssb data. Can be set to
#' 0 (zero) if ssb-recruitemnt pairs already properly aligned.
#' @param col_names vector of column names for year, recruitment and spawning stock
#' biomass (in that order).
#' @param opt_sr_model \emph{integer}. Number (1-3) corresponding to recruitment model.
#' @param opt_sim \emph{integer}. If 0 no simulation, if 1 simulation.
#' #' @param opt_pen \emph{integer}. If 0 then no recruitement constraints if 1
#' then recruitment contraints applied (NEED MORE DETAIL)
#' @param opt_age \emph{integer}. If 0 then error only in recruitment, if 1 then
#' both error in recruitment and steady-state vectors (age based inputs).
#' @param rba data.frame that contains age based input NOT YET IMPLEMENTED
#' @param filename_age \emph{character}. Name of the option file that contains the
#' age-based-data. If missing, set to "age.dat".
#' @param aP \emph{integer}. Plus group age.
#' @param years vector containing years to include in the ssb-r
#' @param align_rectruitment XXX


write.srmsymc <- function(rby,iba,aR,col_names=c("year","r","ssb"),opt_sr_model=1,
                          opt_sim=1,opt_pen=1,opt_age=1,rba,
                          filename_age="age.dat",aP,years, align_rectruitment=TRUE) {
  
  value <- NULL
  
  ## rby tests
  if(missing(rby)) stop("summary (year) data must be specified")    
  class2 <- rby$info$class2
  if(is.null(class2)) stop("Not implemented yet for provided file type")
  if(class2 != "rby") stop("Not implemented yet for provided file type")
  
  x <- rby$data[,col_names]
  y <- rby$info
  ## rba test
  #if(missing(rba)) stop("prediction (age) data must be specified")
  #class2 <- attributes(rba)$class2
  #if(is.null(class2)) stop("Not implemented yet for provided file type")
  #if(class2 != "rby") stop("Not implemented yet for provided file type")
  
  if(missing(filename_age)) filename_age  <- "age.dat"
  
  ### A. Setting up the srmsymc.dat
  
  ## setting stock name
  name_stock  <- y$name_stock
  if(is.na(name_stock)) name_stock <- "nn"
  name_stock <- y$name_stock <- str_replace_all(name_stock," ","")
  
  aR <- y$time[3]
  if(is.na(aR)) stop("Recruitment age (aR) must be specified")
  
  ## Align recruitment
  if(align_rectruitment == TRUE) {
    x <- align_ssb_r(x,col.names=names(x),aR)
    x <- x[!is.na(x$r),]
  }
  
  if(missing(years)) {
    y1 <- y$time[1]
    y2 <- y$time[2]
    years <- c(y1:y2)
  } else {
    y$time[1] <- y1 <- min(years)
    y$time[2] <- y2 <- max(years)
  }
  
  x <- x[x$year %in% years,]
  
  if(missing(aP)) stop("Plus group age (aP) must be specified")
  y$time[6] <- aP
  
  # return file
  x <- data.frame(r=x$r,ssb=x$ssb,year=x$year)
  y$filename_age = filename_age
  y$opt_sr_model = opt_sr_model
  y$opt_sim = opt_sim
  y$opt_age = opt_age
  y$opt_pen = opt_pen
  rby <- list(data=x,info=y)
  
  cat_srmsymc.dat(name_stock, filename_age, y1, y2, aR, aP,
                  opt_sr_model, opt_sim, opt_age, opt_pen,
                  r=x$r, ssb=x$ssb, year=x$year)
  
  ### B. Setting up the age.dat
  
  if(iba$info$creator == "created from function fishvise:::read.sen") {
    x <- iba$data
    y <- iba$info
    nFleets <- sum(y$fleets[2:4])
    fleet_Fs <- y$mort[,c(2:4)]
    fleet_Fs_names <- colnames(fleet_Fs)
    weight_names <- str_replace(fleet_Fs_names,"F","W")
    ages <- c(y$time[4]:y$time[5])
    sel <- cvsel <- wgt <- cvwgt <- matrix(NA,nrow=length(ages),ncol=nFleets)
    for (i in 1:nFleets) {
      x1 <- x[x$id %in% fleet_Fs_names[i],]
      x1$value <- x1$value/mean(x1$value[x1$age %in% c(fleet_Fs[1,i]:fleet_Fs[2,i])])
      sel[,i] <- x1[,'value']
      cvsel[,i] <- x1[,'cv']
      x1 <- x[x$id %in% weight_names[i],]
      wgt[,i] <- x1[,"value"]
      cvwgt[,i] <- x1[,"cv"]
    }
    bio <- cvbio <- matrix(NA,nrow=length(ages),ncol=3)
    bio[,1]   <- x[x$id %in% 'M','value']
    bio[,2]   <- x[x$id %in% 'xN','value']
    bio[,3]   <- x[x$id %in% 'wN','value']
    cvbio[,1] <- x[x$id %in% 'M','cv']
    cvbio[,2] <- x[x$id %in% 'xN','cv']
    cvbio[,3] <- x[x$id %in% 'wN','cv']
    cat_age.dat(filename_age,nFleets,y$pF,y$pM,sel,cvsel,wgt,cvwgt,bio,cvbio)
    iba <- list(sel=sel,sel_cv=cvsel,w=wgt,w_cv=cvwgt,bio=bio,bio_cv=cvbio,info=y)
  }
  
  if(iba$info$creator == "fishvise::read.rbya") {
    
    x <- iba$data[,c("year","age","tF","wC","wX","xN")]
    x$tF[is.na(x$tF)] <- 0
    x$wC[is.na(x$wC)] <- 0
    x$wX[is.na(x$wX)] <- 0
    x$xN[is.na(x$xN)] <- 0
    ctr <- iba$info
    tF <- ddply(x[x$age %in% ctr$mort[1,1]:ctr$mort[2,1],],"year",summarise,refF=mean(tF))
    x <- join(x,tF)
    x$sF <- x$tF/x$refF
    
    d <- melt(x[,c("year","age","sF","wC","wX","xN")],id.vars=c("year","age"))
    d <- ddply(d,c("variable","age"),summarise,ave=mean(value,na.rm=T),cv=sqrt(var(value,na.rm=T))/ave)
    
    nFleets <- 1
    fleet_Fs <- matrix(c(ctr$mort[1,1],ctr$mort[2,1]),ncol=1,nrow=2)
    colnames(fleet_Fs) <- "sF"
    fleet_Fs_names <- colnames(fleet_Fs)
    weight_names <- "wC"
    
    ages <- c(min(x$age):max(x$age))
    sel <- cvsel <- wgt <- cvwgt <- matrix(0,nrow=length(ages),ncol=nFleets)
    x <- d
    names(x) <- c("id","age","value","cv")
    x$cv[is.na(x$cv)] <- 0
    for (i in 1:nFleets) {
      x1 <- x[x$id %in% fleet_Fs_names[i],]
      x1$value <- x1$value/mean(x1$value[x1$age %in% c(fleet_Fs[1,i]:fleet_Fs[2,i])])
      sel[,i] <- x1[,'value']
      cvsel[,i] <- x1[,'cv']
      x1 <- x[x$id %in% weight_names[i],]
      wgt[,i] <- x1[,"value"]
      cvwgt[,i] <- x1[,"cv"]
    }
    bio <- cvbio <- matrix(0,nrow=length(ages),ncol=3)
    bio[,1]   <- 0.2
    bio[,2]   <- x[x$id %in% 'xN','value']
    bio[,3]   <- x[x$id %in% 'wX','value']
    cvbio[,1] <- 0.0
    cvbio[,2] <- x[x$id %in% 'xN','cv']
    cvbio[,3] <- x[x$id %in% 'wX','cv']
    
    cat_age.dat("age.dat",1,0,0,sel,cvsel,wgt,cvwgt,bio,cvbio) 
    iba <- list(sel=sel,sel_cv=cvsel,w=wgt,w_cv=cvwgt,bio=bio,bio_cv=cvbio,info=y)
  }
  
  
  
  return(list(rby=rby,iba=iba))
}



#' @title Write age.dat to disk
#' 
#' @description XXX
#' 
#' @author Einar Hjorleifsson
#' 
#' @export
#' 
#' @param filename_age XXX
#' @param n_fleets XXX
#' @param pf XXX
#' @param pm XXX
#' @param sel XXX
#' @param sel_cv XXX
#' @param w XXX
#' @param w_cv XXX
#' @param bio XXX
#' @param bio_cv XXX
#' 
cat_age.dat <- function(filename_age,n_fleets,pf,pm,sel,sel_cv,w,w_cv,bio,bio_cv) {
  
  n <- 4 # number of digits to write
  
  tmpfile <- file(filename_age,open='w')
  cat('#Header: Some nice description\n',file=tmpfile,append=TRUE)
  cat(n_fleets,  '# fno: Number of fleets (nstocks)\n',file=tmpfile,append=TRUE)
  cat(1,      '# sno: Fleets for yield per recruit stats (always 1)\n',file=tmpfile,append=TRUE)
  cat(pf,   '# f: proportional fishing mortality before spawning time (pf)\n',file=tmpfile,append=TRUE)
  cat(pm,   '# m: proportional natural mortality before spawning time (pm)\n',file=tmpfile,append=TRUE)
  cat('# Selection pattern\n',file=tmpfile,append=TRUE)
  for(i in 1:nrow(sel))   cat(format(round(sel[i,],n),n),'\n',file=tmpfile,append=TRUE)
  cat('# cv Selection pattern\n',file=tmpfile,append=TRUE)
  for(i in 1:nrow(sel_cv)) cat(format(round(sel_cv[i,],n)),'\n',file=tmpfile,append=TRUE)
  cat('# Weight at age\n',file=tmpfile,append=TRUE)
  for(i in 1:nrow(w))   cat(format(round(w[i,],n),n),'\n',file=tmpfile,append=TRUE)
  cat('# cv Weight at age\n',file=tmpfile,append=TRUE)
  for(i in 1:nrow(w_cv)) cat(format(round(w_cv[i,],n),n),'\n',file=tmpfile,append=TRUE)
  cat('# Biological data\n',file=tmpfile,append=TRUE)
  cat('#    M,     mat,   wSSB\n',file=tmpfile,append=TRUE)
  for(i in 1:nrow(bio))   cat(format(round(bio[i,],n),n),'\n',file=tmpfile,append=TRUE)
  cat('# cv Biological data\n',file=tmpfile,append=TRUE)
  cat('#  cvM,   cvmat, cvwSSB\n',file=tmpfile,append=TRUE)
  for(i in 1:nrow(bio_cv)) cat(format(round(bio_cv[i,],n),n),'\n',file=tmpfile,append=TRUE)
  close(tmpfile)
}

#' @title Write srmsymc.dat to disk
#' 
#' @description XXX
#' 
#' @author Einar Hjorleifsson
#' 
#' @export
#' 
#' @param name_stock XXX
#' @param filename_age XXX
#' @param y1 XXX
#' @param y2 XXX
#' @param aR XXX
#' @param aP XXX
#' @param opt_sr_model XXX
#' @param opt_sim XXX
#' @param opt_age XXX
#' @param opt_pen XXX
#' @param r XXX
#' @param ssb XXX
#' @param year XXX

cat_srmsymc.dat <- function(name_stock, filename_age, y1, y2, aR, aP,
                            opt_sr_model, opt_sim, opt_age, opt_pen,
                            r, ssb, year) {
  tmpfile <- file('srmsymc.dat',open='w')
  cat('# Header: Some nice description\n',file=tmpfile,append=TRUE)
  cat(name_stock, '# stkname: Name of the stock\n',file=tmpfile,append=TRUE)
  cat(filename_age,'# filname: Name of the option file (2nd file\n',file=tmpfile,append=TRUE)
  cat(y1,    ' # ybeg:   First year                                                   (yearRange[1])\n',file=tmpfile,append=TRUE)
  cat(y2,    ' # yend:   Last year                                                    (yearRange[2])\n',file=tmpfile,append=TRUE)
  cat(aR,    ' # r:      Recruitment age                                              (senhead[1])\n',file=tmpfile,append=TRUE)
  cat(aP,    ' # A:      Plust group age                                              (senhead[2])\n',file=tmpfile,append=TRUE)
  cat(opt_sr_model,  ' # Ropt:   S-R function type                                            (sr)\n',file=tmpfile,append=TRUE)
  cat(opt_sim,' # simopt: 0=no simulation, 1=simulation                                (ifelse(nits==0,0,1)) \n',file=tmpfile,append=TRUE)
  cat(opt_age,' # senopt: 0=error only in recr, 1=error in recr & steady-state vectors (ifelse(varybiodata,1,0))\n',file=tmpfile,append=TRUE)
  cat(opt_pen,' # penopt: 0=no SR constraints, 1=apply SR constraints                  (ifelse(srconstrain,1,0))\n',file=tmpfile,append=TRUE)
  cat('# r ssb\n', file=tmpfile, append=TRUE)
  cat(paste(r,ssb,'#',year), file = tmpfile,append = TRUE,sep="\n")
  close(tmpfile)
}

