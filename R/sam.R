

#' @title get_fit_sam
#' 
#' @description Gets the sam output file from the www.stockassessment.org
#' 
#' @export
#' 
#' @param assessment The directory name of the assessment as specified on
#' www.stockassessment.org
#' @param user User name, guest users can use "user3" (default)
get_fit_sam <- function(assessment="wbcod_2014",user="user3") {
  Path <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",
                user,
                assessment,
                "run/",sep="/")
  get_file("sam.par",Path)
  get_file("sam.rep",Path)
  get_file("sam.res",Path)
  get_file("sam.cor",Path)
  get_file("confclone.log",Path)
}





#' @title read_fit_sam
#' 
#' @description Reads the stock assessment summary statistics from the
#' stockassessment.org directory structure
#' 
#' This function originally in scr/common.R as read.fit
#' 
#' @export
#' 
#' @param file Path to the directory containing sam run. Normally ends with
#' ../run/sam
#' @param reduced Boolean, if FALSE (default) ....
#' 
#' @return Returns a list
#' 
#' @note TO DO: Ideally should convert the data to FLStock object and from there
#' convert the stuff to e.g. rby or rbya.
#' 
#' TO DO: Provide detail description of the list returned
#' 
read_fit_sam <- function(file, reduced=FALSE){
  # Function to read a basic fit
  ret <- list()
  parfile <- as.numeric(scan(paste(file,'.par', sep=''), 
                             what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar <- as.integer(parfile[1])
  ret$nlogl <- parfile[2]
  ret$maxgrad <- parfile[3]
  rep<-scan(paste(file,'.rep', sep=''), quiet=TRUE)
  ret$res<-read.table(paste(file,'.res', sep=''),header=FALSE)
  ret$stateDim<-rep[1]
  ret$years<-rep[-1]
  file<-paste(file,'.cor', sep='')
  lin<-readLines(file)
  ret$npar<-length(lin)-2
  ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin<-lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!=''])
  ret$names<-unlist(lapply(sublin,function(x)x[2]))
  ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))
  
  ret$cor<-matrix(NA, ret$npar, ret$npar)
  for(i in 1:ret$npar){
    ret$cor[1:i,i]<-as.numeric(unlist(lapply(sublin[i],
                                             function(x)x[5:(4+i)])))
    ret$cor[i,1:i]<-ret$cor[1:i,i]
  }
  ret$cov<-ret$cor*(ret$std%o%ret$std)
  
  mslh <- function(name){
    idx<-which(ret$names==name)
    x<-cbind(ret$est[idx], ret$std[idx], ret$est[idx]-2*ret$std[idx], 
             ret$est[idx]+2*ret$std[idx])
    colnames(x)<-c('est', 'std', 'low', 'hig')
    return(x)
  }
  
  ret$ssb<-mslh('ssb')
  ret$fbar<-mslh('fbar')
  ret$tsb<-mslh('tsb')
  ret$logssb<-mslh('logssb')
  ret$logfbar<-mslh('logfbar')
  ret$logtsb<-mslh('logtsb')
  ret$logscale<-mslh('logScale')
  ret$logFpar<-mslh('logFpar')
  ret$logCatch<-mslh('logCatch')
  x<-mslh('U')
  ret$stateEst<-matrix(x[,1],ncol=ret$stateDim, byrow=TRUE)
  ret$stateStd<-matrix(x[,2],ncol=ret$stateDim, byrow=TRUE)
  ret$stateLow<-matrix(x[,3],ncol=ret$stateDim, byrow=TRUE)
  ret$stateHig<-matrix(x[,4],ncol=ret$stateDim, byrow=TRUE)
  ret$R<-cbind(exp(ret$stateEst[,1]), NA, exp(ret$stateLow[,1]), 
               exp(ret$stateHig[,1]))
  if(reduced){
    ret <- ret[which(!names(ret)%in%c('cov','cor'))]
  }
  
  file <- sub('[[:alpha:]]+\\.cor$','confclone.log',file)
  if(file.exists(file)){
    ret$keys<-read_conf_sam(file)
  }
  return(ret)
}



#' @title read_rbya_sam
#' 
#' @description read fishing mortality and stock in numbers from sam
#' 
#' @export 
#' 
#' @param x Object from read_fit_sam
#' @param Scale A value 
#' 
read_rbya_sam <- function(x,Scale=1) {
  
  minAge <- min(x$res[,3])  
  maxAge <- max(x$res[,3][x$res[,3]<98.5])  
  noN <- maxAge - minAge+1
  noFleet <- max(x$res[,2])
  
  N <- exp(x$stateEst[,1:noN])/Scale
  colnames(N) <- c(minAge:maxAge)
  rownames(N) <- x$years
  N <- reshape2::melt(N,factorsAsStrings = FALSE)
  
  mort <- exp(x$stateEst[,-c(1:noN)])[,x$keys$keyLogFsta[1,]]
  colnames(mort) <- c(minAge:maxAge)
  rownames(mort) <- x$years
  mort <- reshape2::melt(mort,factorsAsStrings = FALSE)
  
  res <- cbind(N,mort[,3])
  names(res) <- c("year","age","n","f")
  
  # Here we need to read in the input file by read_ibya_lowestoft
  # And join that with the results.
  return(res)
}

#' @title read_rby_sam
#' 
#' @description Reads the stock summary numbers by year from \code{sam}.
#' 
#' @export 
#' 
#' @param x Object from function \code{read.fit}.
#' @param range Boolean Not used
#' @param Scale A value
#' @param format Character string. If "wide" (default) returns a wide table.
#' If "long", returns a data.frame containing year, variable and est, hig and low
#' for each
read_rby_sam <- function(x,range=FALSE,Scale=1,format="wide") {  
  rby <- cbind(x$fbar[,c(1,3,4)],
               x$ssb[,c(1,3,4)],
               x$tsb[,c(1,3,4)],
               x$R[,c(1,3,4)],
               rbind(exp(x$logCatch[,c(1,3,4)]),c(NA,NA,NA)))
  colnames(rby)[10:12] <- c("est","low","hig")
  rby[,4:15] <- rby[,4:15]/Scale
  colnames(rby) <- paste(c(rep("f",3),rep("ssb",3),rep("tsb",3),rep("rec",3),rep("yield",3)),
                         colnames(rby),sep="")
  rby <- data.frame(rby)
  rby$year <- x$years
  rownames(rby) <- NULL
  
  if(format=="long") {
    rby <- reshape2::melt(rby,c("year"))
    rby$variable <- as.character(rby$variable)
    rby$what <- substr(rby$variable,nchar(rby$variable)-2,nchar(rby$variable))
    rby$variable <- substr(rby$variable,1,nchar(rby$variable)-3)
    rby <- reshape2::dcast(rby,year + variable ~ what,value.var = "value")
  }
  
  return(rby)
}



#' @title read_conf_sam
#' 
#' @description Reads sam configuration file from the stockassessment.org
#' directory structure. 
#' 
#' Used by read.fit function (see below). This function originally in
#' scr/common.R named read.conf
#' 
#' @export
#' 
#' @param clonefile XXX
read_conf_sam <- function(clonefile){
  lin <- readLines(clonefile)
  idxNam <- grep("^[[:alpha:]]",lin)
  doone <- function(i){
    idxNam <- c(idxNam,length(lin)+1)
    x <- read.table(textConnection(lin[(idxNam[i]+1):(idxNam[i+1]-1)]))
    names(x) <- NULL
    as.matrix(x)
  }
  ret<-lapply(1:length(idxNam),doone)
  names(ret)<-sub(' =','',lin[idxNam])
  ret
}
