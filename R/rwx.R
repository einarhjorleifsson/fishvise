################################################################################
# Read Results By Year

#' @title Reads results-by-year (sum file)
#' 
#' @description Reads a stock summary file in table format and returns it as
#' a data frame.
#' 
#' @export
#' 
#' @param file If it does not contain an absolute path, the file name is
#' relative to the current working directory, getwd().
#' @param format Character specifying the origin of the input file. So far
#' implemented "rvk" and "abd" format.
#' @param cn Names of columns to return for "rvk" format.
#' @param  info boolean. If FALSE (default) only data.frame with XXX returned. If
#' TRUE a list containing the data.frame and a list containing the additional
#' information is returned. See return for additional XXX.
#' @param name_stock character. Stock name.
#' @param yAss integer. Assessment year
#' @param aR integer. Age of recruitment.
#' @param aF vector. Elements contain youngest and oldest age used to 
#' calculate the reference fishing mortality.
#' @param Run Run name (spring survey, fall survey, both surveys, etc ..)
#' @param Model Model name (Adcam, Adapt, Separable, etc ..)
#' @examples
#' 
#' file <- paste(path.package("fishvise"),'extdata/resultsbyyear',sep='/')
#' read_rby(file)
#' 
#' file <- paste(path.package("fishvise"),'extdata/NSH.sum',sep='/')
#' read_rby(file,format="abd")
#' 

read_rby <- function (file,
                      format="rvk",
                      cn=c("year","r","refB","ssb","oY","pY","refF","hr","oI1","pI1","oI2","pI2"),
                      info=FALSE,
                      name_stock = NA, yAss = NA,aR = NA,aF =c(NA,NA),Run =NA,Model = NA) {
  
  
  
  # 1. file exist check
  
  if(!file.exists(file)) stop(paste("file",file,"not found"))
  
  # 2.a Read "rvk" format
  ## Make a separate subfunction read_rby_rkv
  if(format == "rvk") {
    x <- read.table(file,header=TRUE,na.strings=c("-1","0"))
    # Change names to standard names
    names(x) <- cn_keys$rvk$id[match(names(x),cn_keys$rvk$adx)]
    x <- x[,cn]
  }
  
  # 2.b Read "abd" format
  ## Make a separate subfunction read_rby_adb
  if(format == "abd") {
    x <- read.table(file,skip=27,stringsAsFactors=FALSE)
    names(x) <- c("year","r","ssb","tsb","tYo","hYo","dYo","iYo","tF","hF","dF","iF")
    x <- x[,c(1,2,4,3,5:ncol(x))]
  }
  
  # 2.c. Read "ICES" format
  ## Make a separated subfunction read_rby_ices
  #   Note: Have the old and the new format
  if(format == "ices") {
  }
  
  # 2.d READ "sam" format
  if(format == "sam") {
    x <- read_fit_sam(file, reduced=FALSE)
  }
  
  # 3.
  if(!info) {
    # a. IF info = FALSE, return data.frame
    return(x)
    # b. If info = TRUE, populate attributes and return list
  } else {
    y <- standard_attribute_list()
    y$class2 <- "rby"
    y$creator <- "fishvise::read_rby"
    y$name_stock <- name_stock
    
    if(format == "rvk") {
      y$fleets[1] <- 1
      y$time[1] <- min(x$year)
      y$time[2] <- max(x$year)
      y$time[3]    <- aR
      y$mort[1,1]  <- aF[1]
      y$mort[2,1]  <- aF[2]
    }
    
    if(format == "abd") {
      y$fleets <- c(NA, scan(file, skip = 2, nlines = 1, quiet = TRUE))
      y$time[1:2] <- scan(file, skip = 4, nlines = 1, sep = "", quiet = TRUE)
      tmp <- scan(file, skip = 6, nlines = 1, sep = "", quiet = TRUE)
      y$time[3] <- tmp[1]
      y$units_y[1] <- tmp[2]
      for (i in seq(8, 18, by = 2)) {
        y$units_y[i/2 - 2] <- scan(file, skip = i, nlines = 1, sep = "", quiet = TRUE)
      }
      for (i in seq(20, 26, by = 2)) {
        y$mort[, i/2 - 9] <- scan(file, skip = i, nlines = 1, sep = "", quiet = TRUE)
      }
    }
    
    y$yAss       <- yAss
    y$Run        <- Run
    y$Model      <- Model
    
    return(list(data=x,info=y))
  }
}


#  @title read_rby_rvk

#  @title read_rby_abd

#  @title read_rby_ices

#  @title read_rby_tasac





################################################################################
# Read Results By Year - support functions





#' @title Standard attribute structure
#' 
#' @description XXX
#' 
#' @author Einar Hjorleifsson
#' 
#' @export
#' 
#' 
standard_attribute_list <- function() {
  
  time <- rep(NA,6)
  names(time) <- c("y1","y2","aR","a1","a2","aP")
  
  units_y <- rep(NA,7)
  names(units_y) <- c("r","ssb","tsb","tYo","hYo","dYo","iYo")
  
  mort <- matrix(NA,nrow=2,ncol=4)
  colnames(mort) <- c("tF","hF","dF","iF")
  rownames(mort) <- c("a1","a2")
  
  fleets <- rep(NA,4)
  names(fleets) <- c("total","human","discard","industrial")
  
  yAss <- NA
  yAss <- "Assessment year"
  
  x <- list(class2=NA,creator=NA,name_stock="nn",fleets=fleets,time=time,
            mort=mort,pM=NA,pF=NA,units_y=units_y,Run=NA,Model=NA,yAss=yAss)
  
  return(x)
}




################################################################################
# Read results by year and age

#' @title Reads results-by-year-and-age
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param file If it does not contain an absolute path, the file name is
#' relative to the current working directory, getwd().
#' @param format Character specifying the origin of the input file. So far
#' implemented "rvk" and "abd" format.
#' @param cn Names of columns to return for "rvk" format.
#' @param  info boolean If FALSE (default) only data.frame with XXX returned. If
#' TRUE a list containing the data.frame and a list containing the additional
#' information is returned. See return for additional XXX.
#' @param name_stock character. Stock name.
#' @param yAss integer. Assessment year
#' @param aR integer. Age of recruitment.
#' @param aP integer. Plus group age
#' @param aF vector. Elements contain youngest and oldest age used to 
#' calculate the reference fishing mortality.
#' @param pM XXX
#' @param pF XXX
#' @param Run Run name (spring survey, fall survey, both surveys, etc ..)
#' @param Model Model name (Adcam, Adapt, Separable, etc ..)
#' @examples
#' 
#' file <- paste(path.package("fishvise"),'extdata/resultsbyyearandage',sep='/')
#' read_rbya(file)
#' 

read_rbya <- function (file,
                      format="rvk",
                      cn=c("xx"),
                      info=FALSE,
                      name_stock = NA, yAss = NA,aR = NA, aP = NA, aF =c(NA,NA),
                      pM = rep(0.25,14), pF = rep(0.1,14),
                      Run =NA,Model = NA) {
  
  # 1. file exist check
  if(!file.exists(file)) stop(paste("file",file,"not found"))
  
  
  # 2.a Read "rvk" format
  if(format == "rvk") {
    x <- read.table(file,header=TRUE,na.strings=c("-1","0"))
    # Change names to standard names
    names(x) <- cn_keys$rvk$id[match(names(x),cn_keys$rvk$adx)]
    #x <- x[,cn]
  }
  
  # 2.b Read "sam" format
  if (format == "sam") {
    x <- read_rbya_sam(read_fit_sam(file))
    
  }
  
  
  # 3.
  if(!info) {
    # a. IF info = FALSE, return data.frame
    return(x)
    # b. If info = TRUE, populate attributes and return list
  } else {
    y <- standard_attribute_list()
    y$class2 <- "rbya"
    y$creator <- "fishvise::read_rbya"
    y$name_stock <- name_stock
    
    if(format == "rvk") {
      y$fleets[1] <- 1
      y$time[1] <- min(x$year)
      y$time[2] <- max(x$year)
      y$time[3] <- aR
      y$time[4] <- min(x$age)
      y$time[5] <- max(x$age)
      y$time[6] <- aP
      y$mort[1,1]  <- aF[1]
      y$mort[2,1]  <- aF[2]
      y$pM <- pM
      y$pF <- pF
    }
    
    if(format == "xxx") {
    }
    
    y$yAss       <- yAss
    y$Run        <- Run
    y$Model      <- Model
    
    return(list(data=x,info=y))
  }
}









#' @title Read .sen file
#' 
#' @description Reads a file of the .sen format (Aberdeen format) and creates a
#' \code{data.frame}. Additional informations are stored as
#' attributes.
#' @author Einar Horleifsson <einar.hjorleifsson@@gmail.com>
#' 
#' @export
#' 
#' @param file Name of the .sen file
#' @param pM vector containing proportional natural mortality before spawning
#' @param pF vector containing proportional fishing mortality before spawning

read_sen <- function(file,pM,pF) {
  
  if(missing(file)) stop('file must be specified') 
  if (!file.exists(file)) stop(paste('SEN file',file,'not found'))

  # -1. setup
  y <- standard_attribute_list()
  
  if(missing(pM)) pM <- 0
  if(missing(pF)) pF <- 0
  y$pM <- pM
  y$pF <- pF
    
  # 0. Read the whole thing
  tmpfile <-  readLines(file)
  nLines <- length(tmpfile)
  
  # 1. header
  header <- stringr::str_trim(readLines(file,1))
  header <- stringr::str_replace_all(header,"\"","")
  header <- str_trim_tab(header)
  header <- stringr::str_replace(header,"Stock summary, ","")
  y$name_stock <- header
  
  # 2. Mortality specifiations
  x <- scan(file,skip=1,nlines=1,quiet=TRUE,sep=",")
  y$time[4] <- x[1]
  y$time[5] <- x[2]
  
  # unknown/unclarified stuff
  y$yAss <- x[3] # Assessment year ??
  UNKNOWN <- x[4]
  
  # temporary
  ages  <- c(y$time[4]:y$time[5])
  nages <- length(ages)
  
  # 3. line 3
  y$fleets[2:4] <- scan(file,nlines=1,skip=2,quiet=TRUE,sep=',')
  nFleets <- sum(y$fleets[2:4])
  
  # 4. Set up a data.frame for age based values
  d <- expand.grid(age=ages,
                   abd=c("N","sH","sD","sI","WH","WD","WI","WS","M","MT"))
  d$variable <- paste(d$abd,d$age,sep='')
  yAp0 <- substr(y$yAss,3,4)
  yAp1 <- substr(y$yAss+1,3,4)
  yAp2 <- substr(y$tAss+2,3,4)
  d2 <- data.frame(age=rep(NA,8),
                   abd=c("R","R",rep("HF",3),rep("K",3)),
                   variable=c(paste('R',yAp1,sep=''),
                              paste('R',yAp2,sep=''),
                              paste('HF',yAp0,sep=''),
                              paste('HF',yAp1,sep=''),
                              paste('HF',yAp2,sep=''),
                              paste('K',yAp0,sep=''),
                              paste('K',yAp1,sep=''),
                              paste('K',yAp2,sep='')))
  d <- rbind(d,d2)
  
  # 4. Read in age and prediction related stuff
  nRows <- nages + nFleets * nages * 2 + 3*nages + 8
  x <- read.table(file,skip=3,nrows=nRows,sep=',',stringsAsFactors=FALSE)
  names(x) <- c('variable','value','cv')
  d <- plyr::join(d,x,by='variable')
  
  d$value[is.na(d$value)] <- 0
  d$cv[is.na(d$cv)] <- 0
  
  # get in the remainder of the sen file
  n <- 3 + nRows
  
  # mortality specs
  x <- scan(file,nlines=1,skip=n+5,quiet=TRUE,sep=',')
  x <- x[!is.na(x)]
  y$mort[1,2] <- x[1]
  y$mort[2,2] <- x[2]
  
  # input year
  x <- scan(file,nlines=1,skip=n+6,quiet=TRUE,sep=',')
  x <- x[!is.na(x)]
  y$time[1] <- x[1]
  y$time[2] <- x[2]
  
  d <- plyr::join(d,cn_keys$abd)
  d <- d[,c("id","age","value","cv")]
  
  y$creator= "created from function fishvise::read_sen"

  # create "elements"
  
  return(list(data=d,info=y))
}





# Stuff from the surbar package


#' @title Read in Lowestoft-format VPA data
#' 
#' @export
#' @param filename The name of the file to read in
#' @param val.name XXX
#' @param Format The format of the output, available is "List","Wide","Long"
read_lowestoft <- function(filename, val.name, Format="List")
{
  y <- scan(filename, skip = 2, nlines = 1, quiet = TRUE)
  a <- scan(filename, skip = 3, nlines = 1, quiet = TRUE)
  tab <- read.delim(filename, header = FALSE, sep = "", skip = 5)[1:(y[2] - y[1] + 1),]
  names(tab) <- c(a[1]:a[2])
  rownames(tab) <- c(y[1]:y[2])
  if(Format == "List") return(list(y = y, a = a, tab = tab))
  if(Format == "Wide") return(tab)
  tab$year <- as.integer(rownames(tab))
  tab <- reshape2::melt(tab,id.vars="year",factorsAsStrings = FALSE)
  names(tab) <- c("year","age",val.name)
  tab$age <- as.integer(as.character(tab$age))
  tab <- tab[!is.na(tab$age),]
  tab[,3] <- as.numeric(tab[,3])
  return(tab)
}

#' @title read_ibya_lowestoft
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param path XXX
#' @param Scale XXX
read_ibya_lowestoft <- function(path,Scale=1) {
  oc <-  read_lowestoft(paste(path,"cn.dat",sep="/"),val.name="oC",Format = "Long")
  oc$oC <- oc$oC/Scale
  cw <-  read_lowestoft(paste(path,"cw.dat",sep="/"),val.name="cW",Format = "Long")
  sw <-  read_lowestoft(paste(path,"sw.dat",sep="/"),val.name="sW",Format = "Long")
  mat <- read_lowestoft(paste(path,"mo.dat",sep="/"),val.name="mat",Format = "Long")
  nat <- read_lowestoft(paste(path,"nm.dat",sep="/"),val.name="m",Format = "Long")
  pf <-  read_lowestoft(paste(path,"pf.dat",sep="/"),val.name="pf",Format = "Long")
  pm <-  read_lowestoft(paste(path,"pm.dat",sep="/"),val.name="pm",Format = "Long")
  
  res <- plyr::join(oc,sw,by=c("year","age"))
  res <- plyr::join(res,cw,by=c("year","age"))
  res <- plyr::join(res,mat,by=c("year","age"))
  res <- plyr::join(res,nat,by=c("year","age"))
  res <- plyr::join(res,pf,by=c("year","age"))
  res <- plyr::join(res,pm,by=c("year","age"))
  return(res)
}



#dir.data <- "~/ass/2015/01/BenchMark/ass/sam/data"
#file <- file.path(dir.data, "index.txt")






read_ibya_lowestoft2 <- function (file, sep = "", quiet = TRUE) 
{
  if (!file.exists(file)) {
    if (quiet == TRUE) stop()
    if (quiet != TRUE) stop(paste("VPA index file", file, "does not exist"))
  }
  dir <- dirname(file)
  files. <- scan(file, what = "character", skip = 2, sep = sep, 
                 quiet = quiet)
  for (i in seq(length(files.))) {
    if (!grepl(dir, files.[i])) {
      files.[i] <- file.path(dir, files.[i], fsep = .Platform$file.sep)
    }
  }
  
  range1 <- scan(files.[1], skip = 2, nlines = 1, sep = sep, 
                 quiet = quiet)
  range2 <- scan(files.[1], skip = 3, nlines = 1, sep = sep, 
                 quiet = quiet)
  range <- c(range1[1:2], range2[1:2])
  ages <- range[3:4]
  yrs <- range[1:2]
  
  # should have dimention year age, not age year
  a <- array(NA,
             dim = c(length(ages[1]:ages[2]),
                     length(yrs[1]:yrs[2])),
             dimnames=list(age=ages[1]:ages[2],
                           year=yrs[1]:yrs[2]))
  
  for (i in files.) {
    if (!file.exists(i)) {
      if (quiet != TRUE) 
        cat("File ", i, "does not exist", "\n")
    }
    if (file.exists(i)) {
      a. <- readVPAFile(i, sep = sep, quiet = quiet)
      switch(as.character(scan(i, skip = 1, nlines = 1, 
                               sep = sep, comment.char = "#", quiet = TRUE)[2]),
             `1` = FLStock.@landings <- a., `2` = FLStock.@landings.n <- a., 
             `3` = FLStock.@landings.wt <- a., `4` = FLStock.@stock.wt <- a., 
             `5` = FLStock.@m <- a., `6` = FLStock.@mat <- a., 
             `7` = FLStock.@harvest.spwn <- a., `8` = FLStock.@m.spwn <- a., 
             `21` = FLStock.@discards <- a., `22` = FLStock.@discards.n <- a., 
             `23` = FLStock.@discards.wt <- a., `24` = FLStock.@catch <- a., 
             `25` = FLStock.@catch.n <- a., `26` = FLStock.@catch.wt <- a., 
             `27` = FLStock.@harvest <- a., `28` = FLStock.@stock.n <- a.)
    }
  }
  FLStock.@range <- c(min = ages[1], max = ages[2], plusgroup = ages[2], 
                      minyear = yrs[1], maxyear = yrs[2])
  FLStock.@desc <- paste("Imported from a VPA file (", file, 
                         "). ", date(), sep = "")
  FLStock.@name <- scan(file, nlines = 1, what = character(0), 
                        sep = "\n", quiet = TRUE)
  return(FLStock.)
}


#' @title Read in Lowestoft-format survey data
#' 
#' @export
#' @param filename The name of the file to read in
read_lowestoft_survey <- function(filename)
{
  n <- scan(filename, skip = 1, nlines = 1, quiet = TRUE) - 100
  idx <- vector("list", length = n)
  start <- 3
  for (k in 1:n)
  {
    idx[[k]]$name <- paste(scan(filename, skip = start - 1, nlines = 1, 
                                      what = character(0), quiet = TRUE), collapse = " ")
    temp <- scan(filename, skip = start, nlines = 1, quiet = TRUE)
    idx[[k]]$y1 <- temp[1]
    idx[[k]]$y2 <- temp[2]
    idx[[k]]$ny <- temp[2] - temp[1] + 1
    temp <- scan(filename, skip = start + 1, nlines = 1, quiet = TRUE)
    idx[[k]]$rho <- 0.5 * (temp[4] + temp[3])
    temp <- scan(filename, skip = start + 2, nlines = 1, quiet = TRUE)
    idx[[k]]$a1 <- temp[1]
    idx[[k]]$a2 <- temp[2]
    idx[[k]]$na <- temp[2] - temp[1] + 1
    idx[[k]]$tab <- read.table(filename, skip = start + 3, nrows = idx[[k]]$ny)
    temp <- idx[[k]]$tab[,2:(idx[[k]]$na + 1)] 
    effort <- idx[[k]]$tab[,1] 
    idx[[k]]$tab <- data.frame(temp / effort)
    names(idx[[k]]$tab) <- idx[[k]]$a1:idx[[k]]$a2
    rownames(idx[[k]]$tab) <- idx[[k]]$y1:idx[[k]]$y2
    start <- start + 4 + idx[[k]]$ny
  }
  list(n = n, idx = idx)
}

#' Replace characters
#' 
#' \code{replace.text} replaces string with another string. Function was called 'skipta.texta'. In theory could use str_replace instead.
#' 
#' @param txt input vector
#' @param char character to be replaced.
#' @param replchar replacement character.
replace.text <- function (txt, char = "_", replchar = ".") 
{
  backsl <- "\\"
  tmpfile1 <- tempfile("splusskipt")
  tmpfile2 <- tempfile("splusskipt")
  tmpskipanaskra <- tempfile("splusskipun")
  on.exit(unlink(tmpfile1))
  on.exit(unlink(tmpfile2))
  on.exit(unlink(tmpskipanaskra))
  txt <- paste(txt, collapse = "\n")
  write(txt, file = tmpfile1)
  skipun <- paste("sed 's/", backsl, char, "/", backsl, replchar, 
                  "/g' <", tmpfile1, ">", tmpfile2, sep = "")
  write(skipun, file = tmpskipanaskra)
  system(paste("chmod u+x", tmpskipanaskra))
  system(tmpskipanaskra)
  txt <- scan(tmpfile2, what = character(), sep = "\t",quiet=TRUE)
  return(txt)
  #print(skipun)
}

#' Reads prelude-files
#' 
#' \code{read_prelude} reads prelude files. Originally located on package geo, there called s2pre.
#' 
#' @export
#' @param skr is the name of the file
#' @param rownames, default is FALSE
#' @param dots.in.text, default is TRUE
#' @author Hoski, documented by Einar
#' 
read_prelude <- function (skr, rownames = F, dots.in.text = T) 
{
  fields <- count.fields(skr, sep = "\t")
  nrec <- length(fields)
  if (nrec == 2) 
    return(NULL)
  collab <- scan(file = skr, what = character(), sep = "\t", 
                 n = fields[1],quiet=TRUE)
  outp <- read.table(skr, sep = "\t", skip = 2, as.is = T, 
                     row.names = NULL, na.strings = "")
  names(outp) <- collab
  if (rownames) {
    row.names(outp) <- outp[, 1]
    outp <- outp[, 2:ncol(outp)]
  }
  if (dots.in.text) 
    names(outp) <- replace.text(names(outp))
  return(outp)
}

#' Writes prelude-file
#' 
#' \code{write.prelude} writes prelude files
#' 
#' @export
#' @param data data.frame or matrix object
#' @param file Name of output file - default is "R.pre"
#' @param na.replace A character to replace NA with in the output file ("" by default)
#' @return A prelude-file reprsentation of the data object is written to a file
#' @note If file exist it is simply overwritten without any warning
#' @seealso \code{\link{read_prelude}} for reading prelude files
write_prelude <- function (data, file = "R.pre", na.replace = "")
{
  if (is.data.frame(data)) 
    data <- as.matrix.data.frame(data)
  data[is.na(data) | data == "NA"] <- na.replace
  col.names <- dimnames(data)[[2]]
  if (is.null(col.names) || length(col.names) == 0) 
    col.names <- paste("dalkur", 1:ncol(data), sep = "")
  row.names <- dimnames(data)[[1]]
  if (!is.null(row.names) && length(row.names) > 0) {
    col.names <- c("linu_nofn", col.names)
    data <- cbind(row.names, data)
  }
  n.of.col <- length(col.names)
  cat(col.names, sep = c(rep("\t", n.of.col - 1), "\n"), file = file)
  strika.lina <- rep("", n.of.col)
  for (i in 1:n.of.col) strika.lina[i] <- paste(rep("-", nchar(col.names[i])), 
                                                collapse = "")
  cat(strika.lina, sep = c(rep("\t", n.of.col - 1), "\n"), 
      file = file, append = T)
  cat(t(data), sep = c(rep("\t", n.of.col - 1), "\n"), file = file, 
      append = T)
  return(invisible(NULL))
}


#' Reads 'adcam' assessment results
#' 
#' Some longer text here
#' 
#' @export
#' @param path Path to the runs, here the 'root' path to the runs.
#' @param run Name of the \emph{directory} that contains the result.
#' @param rName Name of the run.
#' @param mName Name of the model used.
#' @param calcSurBio Flag, TRUE (default) if survey biomass should be calculated.
#' @param ggFactor If TRUE (default) rescale prerecruits with M=0.0
#' @param Scale Convertion of values
#' @param assYear Assessment year
#' @param retroY The retrospective year
#' @return A list with \code{data.frame} rby, rbya and rba 
#' @seealso \code{\link{read_separ}} for reading separate model output and \code{\link{read_adapt}} for reading adapt model output
read_adcam <- function (path,run,rName=NA,mName=NA,calcSurBio=T,ggFactor=T,Scale=1e3,assYear=NA,retroY=NA) {
  cnRby <- c("year","r","n3","n6","bioF","bio","bio1","ssb","ssb2","fbar","hr",
             "oY","pY","oU1","pU1","oU2","pU2","run","model")
  cnRbya <- c("year","age","oC","cW","sW","ssbW","mat","n","z","f","m",
              "pC","rC","oU1","pU1","rU1","oU2","pU2","rU2")
  cnRba <- c("age","sel","pSel","sigma","cvU1","qU1","pU1","cvU2","qU2","pU2","run","model")
  # rby
  if(is.na(retroY)) rby <- read.table(paste(path,run,"resultsbyyear",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rby <- read.table(paste(paste(path,run,"resultsbyyear",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rby)
  if(ncol(rby)!=21) {
    rby$pU2 <- NA
    rby$oU2 <- NA
    rby <- rby[,c(1:17,20:21,18:19)]
  }
  names(rby) <- c("year","F2","fbar","pY","ssb","ssb2","bioF","bio",
                  "bio1","r","n3","n6","hr","oY","a50","pU1","oU1",
                  "pU2","oU2","n","qF")
  
  rby$r <- rby$r/Scale
  if(ggFactor) rby$r <- rby$r*exp(-0.4)
  rby$r <- c(rby$r[2:nrow(rby)],NA)
  rby$hr <- rby$pY/rby$bio
  rby$run <- rName
  rby$model <- mName
  rby <- rby[,cnRby] 
  rby$n3 <- rby$n3/Scale
  rby$n6 <- rby$n6/Scale
  # rbyaa
  if(is.na(retroY)) rbya <- read.table(paste(path,run,"resultsbyyearandage",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rbya <- read.table(paste(paste(path,run,"resultsbyyearandage",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rbya)
  if(ncol(rbya)<18) {
    if(ncol(rbya)==16) rbya$SSB <- rbya$N*rbya$SSBwts*rbya$StockSexmat/1e6
    rbya$pU2 <- NA
    rbya$oU2 <- NA
    rbya$rU2  <- NA
    rbya <- rbya[,c(1:16,18:20,17)]
  }
  names(rbya) <- c("year","age","n","z","sW","m","f","pC","cW","ssbW","mat",
                   "matC","oC","pU1","oU1","rU1","pU2","oU2","rU2","ssb")
  if(ggFactor) rbya$n <- ifelse(rbya$age %in% 1,rbya$n*exp(-0.4),rbya$n)
  if(ggFactor) rbya$n <- ifelse(rbya$age %in% 2,rbya$n*exp(-0.2),rbya$n)
  rbya$rC <- log(rbya$oC/rbya$pC)
  rbya <- rbya[,cnRbya]
  rbya$run <- rName
  rbya$model <- mName
  rbya$oC <- rbya$oC/Scale
  rbya$cW <- rbya$cW/Scale
  rbya$sW <- rbya$sW/Scale
  rbya$n  <- rbya$n/Scale
  rbya$pC <- rbya$pC/Scale
  # rba
  if(is.na(retroY)) rba <- read.table(paste(path,run,"resultsbyage",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rba <- read.table(paste(paste(path,run,"resultsbyage",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  } 
  n <- nrow(rba)
  if(ncol(rba)!=11) {
    rba$cvU2 <- rep(NA,n)
    rba$qU2  <- rep(NA,n)
    rba$pU2  <- rep(NA,n)
    rba <- rba[,c(1:5,9:11,6:8)]
  }
  names(rba) <- c("age","m","cvU1","qU1","pU1","cvU2","qU2","pU2","sel","pSel","sigma")
  rba$run <- rName
  rba$model <- mName
  rba <- rba[,cnRba]
  if(!is.na(retroY)) {
    #print(retroY)
    rby$assYear <- as.numeric(retroY)+1
    rbya$assYear <- as.numeric(retroY)+1
    rba$assYear <- as.numeric(retroY)+1
  } else {
    rby$assYear <- assYear
    rbya$assYear <- assYear
    rba$assYear <- assYear
  }
  return(list(rby=rby,rbya=rbya,rba=rba))
}

#' Reads 'adapt' assessment results
#' 
#' Some longer text here
#' 
#' @export
#' @param path Path to the runs, here the 'root' path to the runs.
#' @param run Name of the \emph{directory} that contains the result.
#' @param rName Name of the run.
#' @param mName Name of the model used.
#' @param calcSurBio Flag, TRUE (default) if survey biomass should be calculated.
#' @param ggFactor If TRUE (default) rescale prerecruits with M=0.0
#' @param Scale Convertion of values
#' @param assYear Assessment year
#' @param retroY The retrospective year
#' @return A list with \code{data.frame} rby, rbya and rba 
#' @seealso \code{\link{read_separ}} for reading separate model output and \code{\link{read_adcam}} for reading adcam model output
read_adapt <- function (path,run,rName=NA,mName=NA,calcSurBio=F,ggFactor=T,Scale=1e3,assYear=NA,retroY=NA) {
  cnRby <- c("year","r","n3","n6","bioF","bio","bio1","ssb","ssb2","fbar","hr",
             "oY","pY","oU1","pU1","oU2","pU2","run","model") 
  cnRbya <- c("year","age","oC","cW","sW","ssbW","mat","n","z","f","m",
              "pC","rC","oU1","pU1","rU1","oU2","pU2","rU2")
  cnRba <- c("age","sel","pSel","sigma","cvU1","qU1","pU1","cvU2","qU2","pU2","run","model")  
  
  
  
  #cnRby <- c("year","n1","n3","n6","cBio","bio1","bio2","ssb","ssb2","f","hr",
  #           "oY","preY","obsU1","preU1","obsU2","preU2","run","model")
  #cnRbyaa <- c("year","age","obsC","cW","sW","ssbW","mat","n","z","f","m",
  #              "preC","resC","obsU1","preU1","resU1","obsU2","preU2","resU2")
  #cnRba <- c("age","sel","pSel","sigma","cvU1","qU1","pU1","cvU2","qU2","pU2","run","model")
  
  
  # rby
  if(is.na(retroY)) rby <- read.table(paste(path,run,"resultsbyyear",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rby <- read.table(paste(paste(path,run,"resultsbyyear",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rby)
  if(ncol(rby)!=18) {
    rby$pU2 <- rep(NA,n)
    rby$oU2 <- rep(NA,n)
  }
  names(rby) <- c("year","fbar","pY","oY","ssb","ssb2","bioF","bio1",
                  "bio","preR","r","n1","n3","n6","pU1","oU1",
                  "pU2","oU2")
  if(ggFactor) rby$r <- rby$r*exp(-0.4)
  rby$hr <- ifelse(!is.na(rby$oY),rby$oY,rby$pY)/rby$bio
  rby$run <- rName
  rby$model <- mName
  rby <- rby[,cnRby]
  
  rby$r <- rby$r/Scale
  rby$n3 <- rby$n3/Scale
  rby$n6 <- rby$n6/Scale
  
  
  # rbyaa
  if(is.na(retroY)) rbya <- read.table(paste(path,run,"resultsbyyearandage",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rbya <- read.table(paste(paste(path,run,"resultsbyyearandage",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rby)
  if(ncol(rbya) != 19) {
    rbya$pU2 <- NA
    rbya$oU2 <- NA
    rbya$rU2  <- NA
  }
  names(rbya) <- c("year","age","n","z","sW","m","f","pC","cW","ssbW","mat",
                   "oC","rC","pU1","oU1","rU1","pU2","oU2","rU2")
  if(ggFactor) rbya$n <- ifelse(rbya$age %in% 1,rbya$n*exp(-0.4),rbya$n)
  if(ggFactor) rbya$n <- ifelse(rbya$age %in% 2,rbya$n*exp(-0.2),rbya$n)
  
  rbya <- rbya[,cnRbya]
  rbya$run <- rName
  rbya$model <- mName
  rbya$oC <- rbya$oC/Scale
  rbya$cW <- rbya$cW/Scale
  rbya$sW <- rbya$sW/Scale
  rbya$n  <- rbya$n/Scale
  rbya$pC <- rbya$pC/Scale
  
  # rba
  if(is.na(retroY)) rba <- read.table(paste(path,run,"resultsbyage",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rba <- read.table(paste(paste(path,run,"resultsbyage",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rba)
  if(ncol(rba)!=10) {
    rba$cvU2 <- rep(NA,n)
    rba$qU2  <- rep(NA,n)
    rba$pU2  <- rep(NA,n)
  }
  names(rba) <- c("age","sel","pSel","sigma","cvU1","qU1","pU1","cvU2","qU2","pU2")
  rba$run <- rName
  rba$model <- mName
  rba <- rba[,cnRba]
  if(!is.na(retroY)) {
    print(retroY)
    rby$assYear <- as.numeric(retroY)+1
    rbya$assYear <- as.numeric(retroY)+1
    rba$assYear <- as.numeric(retroY)+1
  } else {
    rby$assYear <- assYear
    rbya$assYear <- assYear
    rba$assYear <- assYear
  }
  
  return(list(rby=rby,rbya=rbya,rba=rba))
}

#' Reads 'separable' assessment results
#' 
#' Some longer text here
#' 
#' @export
#' @param path Path to the runs, here the 'root' path to the runs.
#' @param run Name of the \emph{directory} that contains the result.
#' @param rName Name of the run.
#' @param mName Name of the model used.
#' @param calcSurBio Flag, TRUE (default) if survey biomass should be calculated.
#' @param ggFactor If TRUE (default) rescale prerecruits with M=0.0
#' @param Scale Convertion of values
#' @param assYear Assessment year
#' @param retroY The retrospective year
#' @return A list with \code{data.frame} rby, rbya and rba 
#' @note If file exist it is simply overwritten without any warning
#' @seealso \code{\link{read_adcam}} for reading adcam model output and \code{\link{read_adapt}} for reading adapt model output
read_separ <- read_adapt  # just to avoid confusion


################################################################################
# TASAC stuff - at least works for her-noss

#' @title read_rbya_tasac
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param file Name of the summary.txt file
read_rbya_tasac <- function(file) {
  x <- readLines(file)
  i1 <- grep("N in model:",x)
  i2 <- grep("F in model:",x)
  n <- x[c((i1+1):(i2-2))]
  n[1] <- paste("age",n[1])
  writeLines(n,"tmp.txt")
  rbya <- read.table("tmp.txt",header=T)
  names(rbya) <- stringr::str_replace_all(names(rbya),"X","")
  rbya <- reshape2::melt(rbya,"age",variable.name="year",value.name = "n")
  rbya$year <- as.integer(as.character(rbya$year))
  i1 <- i2
  i2 <- grep("M in model:",x)
  n <- x[c((i1+1):(i2-2))]
  n[1] <- paste("age",n[1])
  writeLines(n,"tmp.txt")
  y <- read.table("tmp.txt",header=T)
  names(y) <- stringr::str_replace_all(names(y),"X","")
  y <- reshape2::melt(y,"age",variable.name="year",value.name = "f")
  y$year <- as.integer(as.character(y$year))
  rbya <- plyr::join(rbya,y)
  return(rbya)
  }
