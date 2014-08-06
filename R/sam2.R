#' @title get_file
#' 
#' @export
#' 
#' @param URL URL path
#' @param File Name of the file
get_file <- function(URL,File)
{
  temporaryFile <- tempfile()
  download.file(paste(URL,File,sep="/"),
                destfile=temporaryFile,
                method="curl",quiet = T)
  return(temporaryFile)
}

#' @title get_sam
#' 
#' @description Gets the input files and sam output files from the www.stockassessment.org
#' 
#' @export
#' 
#' @param assessment The directory name of the assessment as specified on
#' www.stockassessment.org
#' @param user User name, guest users can use "user3" (default)

get_sam <- function(assessment="wbcod_2014",user="user3") {
  
  URL <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",user,assessment,sep="/")
  
  # Get the input files
  # TODO: A loop?
  oc <-  read_lowestoft(get_file(paste(URL,"data",sep="/"),"cn.dat"),val.name="oC",Format = "Long")
  cw <-  read_lowestoft(get_file(paste(URL,"data",sep="/"),"cw.dat"),val.name="cW",Format = "Long")
  sw <-  read_lowestoft(get_file(paste(URL,"data",sep="/"),"sw.dat"),val.name="sW",Format = "Long")
  mat <- read_lowestoft(get_file(paste(URL,"data",sep="/"),"mo.dat"),val.name="mat",Format = "Long")
  nat <- read_lowestoft(get_file(paste(URL,"data",sep="/"),"nm.dat"),val.name="m",Format = "Long")
  pf <-  read_lowestoft(get_file(paste(URL,"data",sep="/"),"pf.dat"),val.name="pF",Format = "Long")
  pm <-  read_lowestoft(get_file(paste(URL,"data",sep="/"),"pm.dat"),val.name="pM",Format = "Long")
  
  ibya <- join(oc,sw,by=c("year","age"))
  ibya <- join(ibya,cw,by=c("year","age"))
  ibya <- join(ibya,sw,by=c("year","age"))
  ibya <- join(ibya,mat,by=c("year","age"))
  ibya <- join(ibya,nat,by=c("year","age"))
  ibya <- join(ibya,pf,by=c("year","age"))
  ibya <- join(ibya,pm,by=c("year","age"))
  
  # Get the results  
  fit <- list()
  
  file <- get_file(paste(URL,"run",sep="/"),"sam.par")
  parfile <- as.numeric(scan(file, 
                             what='', n=16, quiet=TRUE)[c(6,11,16)])
  fit$nopar <- as.integer(parfile[1])
  fit$nlogl <- parfile[2]
  fit$maxgrad <- parfile[3]
  
  file <- get_file(paste(URL,"run",sep="/"),"sam.rep")
  rep <- scan(file, quiet=TRUE)
  
  file <- get_file(paste(URL,"run",sep="/"),"sam.res")
  fit$res <- read.table(file,header=FALSE)
  
  fit$stateDim <- rep[1]
  fit$years <- rep[-1]
  
  file <- get_file(paste(URL,"run",sep="/"),"sam.cor")
  lin <- readLines(file)
  fit$npar <- length(lin)-2
  fit$logDetHess <- as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin <- lapply(strsplit(lin[1:fit$npar+2], ' '),function(x)x[x!=''])
  fit$names <- unlist(lapply(sublin,function(x)x[2]))
  fit$est <- as.numeric(unlist(lapply(sublin,function(x)x[3])))
  fit$std <- as.numeric(unlist(lapply(sublin,function(x)x[4])))
  
  fit$cor<-matrix(NA, fit$npar, fit$npar)
  for(i in 1:fit$npar){
    fit$cor[1:i,i]<-as.numeric(unlist(lapply(sublin[i],
                                             function(x)x[5:(4+i)])))
    fit$cor[i,1:i]<-fit$cor[1:i,i]
  }
  fit$cov<-fit$cor*(fit$std%o%fit$std)
  
  mslh <- function(name){
    idx <- which(fit$names==name)
    x <- cbind(fit$est[idx], fit$std[idx], fit$est[idx]-2*fit$std[idx], 
               fit$est[idx]+2*fit$std[idx])
    colnames(x) <- c('est', 'std', 'low', 'hig')
    return(x)
  }
  
  fit$ssb <- mslh('ssb')
  fit$fbar<-mslh('fbar')
  fit$tsb<-mslh('tsb')
  fit$logssb<-mslh('logssb')
  fit$logfbar<-mslh('logfbar')
  fit$logtsb<-mslh('logtsb')
  fit$logscale<-mslh('logScale')
  fit$logFpar<-mslh('logFpar')
  fit$logCatch<-mslh('logCatch')
  
  x <- mslh('U')
  fit$stateEst <- matrix(x[,1],ncol=fit$stateDim, byrow=TRUE)
  fit$stateStd <- matrix(x[,2],ncol=fit$stateDim, byrow=TRUE)
  fit$stateLow <- matrix(x[,3],ncol=fit$stateDim, byrow=TRUE)
  fit$stateHig <- matrix(x[,4],ncol=fit$stateDim, byrow=TRUE)
  fit$R<-cbind(exp(fit$stateEst[,1]), NA, exp(fit$stateLow[,1]), 
               exp(fit$stateHig[,1]))
  #if(reduced){
  #  fit <- fit[which(!names(fit)%in%c('cov','cor'))]
  #}
  
  file <- get_file(paste(URL,"run",sep="/"),"confclone.log")
  fit$keys <- read_conf_sam(file)
  
  minAge <- min(fit$res[,3])  
  maxAge <- max(fit$res[,3][fit$res[,3]<98.5])  
  noN <- maxAge - minAge+1
  noFleet <- max(fit$res[,2])
  
  N <- exp(fit$stateEst[,1:noN]) #/Scale
  colnames(N) <- c(minAge:maxAge)
  rownames(N) <- fit$years
  N <- melt(N,factorsAsStrings = FALSE)
  names(N) <- c("year","age","n")
  
  mort <- exp(fit$stateEst[,-c(1:noN)])[,fit$keys$keyLogFsta[1,]]
  rownames(mort) <- fit$years
  #mort <- mort[-nrow(mort),]
  colnames(mort) <- sort(unique(ibya$age[ibya$oC > 0]))
  
  mort <- melt(mort,factorsAsStrings = FALSE)
  names(mort) <- c("year","age","f")
  
  rbya <- join(N,mort,by=c("year","age"))
  rbya <- join(rbya,ibya,by=c("year","age"))
  
  return(list(rbya=rbya,fit=fit))
}

#' @title process_error_sam
#' 
#' @description Calculates the sam process error
#' 
#' @export
#' 
#' @param rbya data.frame containing stock in numbers (n), fishing mortality (f) and
#' natural mortality (m) by year (year) and age (age).

process_error_sam <- function(rbya) {
  x <- rbya[,c("year","age","n")]
  x$year <- x$year - 1
  x$age <- x$age - 1
  names(x)[3] <- "n_plus1"
  
  dat <- join(rbya[,c("year","age","n","f","m")],x,by=c("year","age"))
  dat$z_n <- log(dat$n/dat$n_plus1)
  dat$z_f <- dat$f + dat$m
  dat$dm <- dat$z_n - dat$z_f
  dat <- dat[dat$age < (max(dat$age)-1),]
  return(dat)
}
