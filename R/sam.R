#' @title read_sam
#' 
#' @description Reads results from sam. Equivalent to FLSAM::SAM2FLR except
#' returns a list rather than an FLR-object.
#' 
#' @export
#' 
#' @param run.dir Path to which the files are read from.
#' @param ctrl If missing (default) the relevant information are obtained
#' from the files in the run.dir. If specified this must be a FLSAM.control
#' object containing the parameters employed in the assessment. 
#' @param admb.stem The name of the admb executable (ie without the .dat, .exe etc)
#' @param ibya A long data.frame containing input values. If missing (default)
#' the returned rbya data.frame does not contain the full input data.
read_sam <- function (run.dir,ctrl, admb.stem = "sam",ibya)
{
  if(missing(run.dir)) stop("run.dir must be specified")
  
  res <- list()
  rept.fname <- file.path(run.dir, sprintf("%s.rep", admb.stem))
  rept <- scan(rept.fname, what = integer(), quiet = TRUE)
  res$n.states <- rept[1]
  yrs <- rept[-1]
  
  # The control file
  if (missing(ctrl)) {
    res$name <- sprintf("Read from %s", run.dir)
    res$desc <- date()
    res$nohess <- !file.exists(sprintf("%s/%s.cor", run.dir, 
                                       admb.stem))
    mdl.cfg <- readLines(sprintf("%s/model.cfg", run.dir))
    res$range["min"] <- scan(text = mdl.cfg[grep("Min Age", 
                                                 mdl.cfg, ignore.case = TRUE) + 1], quiet = TRUE)
    max.ages <- scan(text = mdl.cfg[grep("Max Age", mdl.cfg, 
                                         ignore.case = TRUE) + 1], quiet = TRUE)
    res$range["max"] <- max.ages[1]
    res$range["plusgroup"] <- ifelse(max.ages[2] == 1, max.ages[1], NA)
    comment.line <- grepl("^ *#", mdl.cfg)
    num.block <- ifelse(!comment.line, cumsum(comment.line), NA)
    state.line <- grep("STATES", mdl.cfg, ignore.case = FALSE)
    state.block <- min(num.block[-(1:state.line)], na.rm = TRUE)
    res$states <- as.matrix(read.table(text = mdl.cfg[which(num.block == state.block)]))
    colnames(res$states) <- res$range["min"]:res$range["max"]
    rownames(res$states) <- c("catch", seq(nrow(res$states))[-1])
    res$range["minyear"] <- min(yrs)
    res$range["maxyear"] <- max(yrs)
    fbar.vals <- scan(text = mdl.cfg[grep("Fbar", mdl.cfg) + 1], quiet = TRUE)
    res$range["minfbar"] <- min(fbar.vals)
    res$range["maxfbar"] <- max(fbar.vals)
  }
  else {
    #admb.stem <- FLSAM:::.get.admb.stem(ctrl)
    if (length(ctrl@sam.binary) == 0) {
      admb.stem <- "sam"
    } else {
      admb.stem <- gsub("\\.exe$", "", basename(ctrl@sam.binary))
    }
    res$name <- ctrl@name
    res$desc <- ctrl@desc
    res$range <- ctrl@range
    res$nohess <- ctrl@nohess
    res$control <- ctrl
    res$states <- ctrl@states
  }
  
  par.fname <- file.path(run.dir, sprintf("%s.par", admb.stem))
  par.hdr <- as.numeric(scan(par.fname, what = "", n = 16, 
                             quiet = TRUE)[c(6, 11, 16)])
  res$nopar <- as.integer(par.hdr[1])
  res$nlogl <- par.hdr[2]
  res$maxgrad <- par.hdr[3]
  res.fname <- file.path(run.dir, "sam.res")
  res$residuals <- read.table(res.fname, header = FALSE, col.names = c("year", 
                                                                       "fleet", "age", "log.obs", "log.mdl", "std.res"))
  res$residuals$fleet <- factor(sprintf("Fleet %s", res$residuals$fleet))
  if (res$nohess) {
    par.txt <- readLines(par.fname)[-1]
    n.lines <- length(par.txt)
    par.chunks <- paste(par.txt[seq(1, n.lines, by = 2)], 
                        par.txt[seq(2, n.lines, by = 2)])
    par.lst <- lapply(par.chunks, function(x) {
      var.name <- gsub("# (.*?):.*", "\\1", x)
      var.vals <- gsub("# .*?:(.*)", "\\1", x)
      var.dat <- scan(con <- textConnection(var.vals), 
                      quiet = TRUE)
      close(con)
      data.frame(name = var.name, value = var.dat, std.dev = NA)
    })
    par.df <- do.call(rbind, par.lst)
    res$params <- par.df
    res$vcov <- matrix(NA)
  } else {
    std.fname <- file.path(run.dir, sprintf("%s.std", admb.stem))
    res$params <- read.table(std.fname, header = FALSE, skip = 1, 
                             col.names = c("index", "name", "value", "std.dev"))
    res$params$index <- NULL
    cor.fname <- file.path(run.dir, sprintf("%s.cor", admb.stem))
    cor.lines.in <- readLines(cor.fname)[-c(1:2)]
    cor.lines <- strsplit(cor.lines.in, " +")
    vcov.dim <- length(cor.lines)
    cor.file.mat <- matrix(as.character(NA), vcov.dim, vcov.dim + 5)
    for (i in 1:vcov.dim) {
      cor.file.mat[i, seq(cor.lines[[i]])] <- cor.lines[[i]]
    }
    cor.mat.lt <- apply(cor.file.mat[, -c(1:5)], 2, as.numeric)
    cor.mat.lt[upper.tri(cor.mat.lt, diag = FALSE)] <- 0
    cor.mat <- cor.mat.lt + t(cor.mat.lt)
    diag(cor.mat) <- diag(cor.mat)/2
    stds <- as.numeric(cor.file.mat[, 5])
    vcov.mat <- cor.mat * (stds %o% stds)
    dimnames(vcov.mat) <- list(cor.file.mat[, 3], cor.file.mat[, 3])
    res$vcov <- vcov.mat
  }
  u <- subset(res$params, name == "U")
  stateEst <- matrix(u$value, nrow = res$n.states, byrow = FALSE, 
                     dimnames = list(state = NULL, year = res$range["minyear"]:res$range["maxyear"]))
  stateSd <- matrix(u$std.dev, nrow = res$n.states, byrow = FALSE, 
                    dimnames = list(state = NULL, year = res$range["minyear"]:res$range["maxyear"]))
  flq <- array(NA,
               dim=c(length(res$range["min"]:res$range["max"]),
                     length(res$range["minyear"]:res$range["maxyear"])),
               dimnames = list(age = res$range["min"]:res$range["max"], 
                               year = res$range["minyear"]:res$range["maxyear"]))
  n.ages <- length(dimnames(flq)$age)
  res$stock.n <- flq
  res$stock.n[, ] <- exp(stateEst[1:n.ages, ])
  #colnames(res$states) <- dimnames(flq)$age
  res$harvest <- flq
  #res$harvest@units <- "f"
  f.stateEst <- stateEst[-c(1:n.ages), , drop = FALSE]
  
  x <- res$states["catch",]
  x <- x[x != 0]
  for (a in names(x)) {
    res$harvest[a, ] <- exp(f.stateEst[x[a], ])
  }
  #info <- data.frame(FLSAM.version = packageDescription("FLSAM")$Version, 
  #                   FLCore.version = packageDescription("FLCore")$Version, 
  #                   R.version = R.version$version.string, platform = R.version$platform, 
  #                   run.date = Sys.time())
  #res$info <- t(info)
  
  rbya <- reshape2::melt(res$stock.n,value.name="n",factorsAsStrings = FALSE)[,c("year","age","n")]
  f <- reshape2::melt(res$harvest,value.name = "f")[,c("year","age","f")]
  rbya <- plyr::join(rbya,f,by=c("year","age"))
  # Residuals and things
  x <- res$residuals
  fleets <- as.character(unique(x$fleet))
  x$obs <- exp(x$log.obs)
  x$pre <- exp(x$log.mdl)
  cn <- c("year","age","obs","pre","std.res")
  x1 <- x[x$fleet == fleets[1],cn]
  names(x1) <- c("year","age","oC","pC","rC")
  rbya <- plyr::join(rbya,x1,by=c("year","age"))
  x1 <- x[x$fleet == fleets[2],cn]
  names(x1) <- c("year","age","oU1","pU1","rU1")
  rbya <- plyr::join(rbya,x1,by=c("year","age"))
  if(length(fleets) > 2) {
    x1 <- x[x$fleet == fleets[3],cn]
    names(x1) <- c("year","age","oU2","pU2","rU2")
    rbya <- plyr::join(rbya,x1,by=c("year","age"))
  } else {
    rbya$oU2 <- rbya$pU2 <- rbya$rU2 <- NA
  }
  
  # here if ibya is included add it
  if(!missing(ibya)) {
    if(class(ibya) == "FLStock") ibya <- FLStock2rbya(ibya)
    cn <- c("year","age","cW","ssbW","mat","m")
    rbya <- plyr::join(rbya,ibya[,cn],by=c("year","age"))
  }
  
  res$rbya <- rbya
  
  return(res)
}


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
#' @param fit Object from read_fit_sam
#' @param Scale A value 
#' 
read_rbya_sam <- function(fit,Scale=1) {
  
  minAge <- min(fit$res[,3])  
  maxAge <- max(fit$res[,3][fit$res[,3]<98.5])  
  noN <- maxAge - minAge+1
  noFleet <- max(fit$res[,2])
  
  N <- exp(fit$stateEst[,1:noN])/Scale
  colnames(N) <- c(minAge:maxAge)
  rownames(N) <- fit$years
  N <- reshape2::melt(N,factorsAsStrings = FALSE)
  
  mort <- exp(fit$stateEst[,-c(1:noN)])[,fit$keys$keyLogFsta[1,]]
  colnames(mort) <- c(minAge:maxAge)
  rownames(mort) <- fit$years
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
#' @param stdev Boolean, if FALSE (default) not included in the output
read_rby_sam <- function(x,range=FALSE,Scale=1,format="wide",stdev=FALSE) {  
  rby <- cbind(x$fbar,
               x$ssb,
               x$tsb,
               x$R)
  colnames(rby)[13:16] <- c("est","std","low","hig")
  rby[,5:16] <- rby[,5:16]/Scale
  colnames(rby) <- paste(c(rep("f",4),rep("ssb",4),rep("tsb",4),rep("rec",4)),
                         colnames(rby),sep="")
  rby <- data.frame(rby)
  rby$year <- x$years
  
  tmp <- exp(x$logCatch)
  tmp <- tmp/Scale
  colnames(tmp) <- paste("yield",colnames(tmp),sep="")
  tmp <- data.frame(tmp)
  tmp$year <- x$years
  
  rby <- plyr::join(rby,tmp,by="year",type = "full")
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
