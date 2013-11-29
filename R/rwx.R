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
#' read.rby(file)
#' 
#' file <- paste(path.package("fishvise"),'extdata/NSH.sum',sep='/')
#' read.rby(file,format="abd")
#' 

read.rby <- function (file,
                      format="rvk",
                      cn=c("year","r","refB","ssb","oY","pY","refF","hr","oI1","pI1","oI2","pI2"),
                      info=FALSE,
                      name_stock = NA, yAss = NA,aR = NA,aF =c(NA,NA),Run =NA,Model = NA) {
  
  
  
  # 1. file exist check
  
  if(!file.exists(file)) stop(paste("file",file,"not found"))
  
  # 2.a Read "rvk" format
  if(format == "rvk") {
    x <- read.table(file,header=TRUE,na.strings=c("-1","0"))
    # Change names to standard names
    names(x) <- cn_keys$rvk$id[match(names(x),cn_keys$rvk$adx)]
    x <- x[,cn]
  }
  
  # 2.b Read "abd" format
  if(format == "abd") {
    x <- read.table(file,skip=27,stringsAsFactors=FALSE)
    names(x) <- c("year","r","ssb","tsb","tYo","hYo","dYo","iYo","tF","hF","dF","iF")
    x <- x[,c(1,2,4,3,5:ncol(x))]
  }
  
  # 3.
  if(!info) {
    # a. IF info = FALSE, return data.frame
    return(x)
    # b. If info = TRUE, populate attributes and return list
  } else {
    y <- standard_attribute_list()
    y$class2 <- "rby"
    y$creator <- "fishvise::read.rby"
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
#' read.rbya(file)
#' 

read.rbya <- function (file,
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
  
  # 2.b Read "xxx" format
  
  # 3.
  if(!info) {
    # a. IF info = FALSE, return data.frame
    return(x)
    # b. If info = TRUE, populate attributes and return list
  } else {
    y <- standard_attribute_list()
    y$class2 <- "rbya"
    y$creator <- "fishvise::read.rbya"
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
#' @author Einar Horleifsson <einar.hjorleifsson@gmail.com>
#' 
#' @export
#' 
#' @param file Name of the .sen file
#' @param pM vector containing proportional natural mortality before spawning
#' @param pF vector containing proportional fishing mortality before spawning

read.sen <- function(file,pM,pF) {
  
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
  header <- str_trim(readLines(file,1))
  header <- str_replace_all(header,"\"","")
  header <- str_trim_tab(header)
  header <- str_replace(header,"Stock summary, ","")
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
  d <- join(d,x,by='variable')
  
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
  
  d <- join(d,cn_keys$abd)
  d <- d[,c("id","age","value","cv")]
  
  y$creator= "created from function fishvise:::read.sen"

  # create "elements"
  
  return(list(data=d,info=y))
}



#' @title Convert reykjavik to flr
#' 
#' @description xx
#' 
#' @export
#' 
#' @param rbx rbx object
#' @param sName character. Name of stock
#' @param sDesc character. Some description
#' @param pf XX
#' @param pm XX
#' @param cF1 XX
#' @param cF2 XX
#' 
rvk2flr <- function(rbx, sName="nn", sDesc="none",pf=0,pm=0,cF1=5,cF2=10) {
  
  rbya <- rbx$rbya
  names(rbya) <- c("year","age","catch.n","catch.wt","stock.wt","stock1.wt",
                   "mat","stock.n","z","harvest","m","Chat","rC","Uobs","Uhat",
                   "Ures","U2obs","U2hat","U2res","run","model","assYear")
  
  a1 <- min(rbya$age)
  a2 <- max(rbya$age)
  nAge <- length(a1:a2)
  
  y1 <- min(rbya$year)
  y2 <- max(rbya$year)
  
  rbya$catch.wt <- ifelse(rbya$catch.wt== -1,0,rbya$catch.wt)
  rbya$stock.wt <- ifelse(rbya$stock.wt== -1,0,rbya$stock.wt)
  #rbya$catch.n  <- ifelse(rbya$catch.n==  -1,0,rbya$catch.n)
  rbya$harvest  <- ifelse(rbya$harvest==  -1,0,rbya$harvest)
  #rbya$m <- ifelse(rbya$age %in% c(1,2),0.0001,rbya$m)
  #rbya$mat <- ifelse(rbya$age %in% c(1,2) | is.na(rbya$mat),0,rbya$mat)
  #rbya <- rbya[rbya$year %in% c(1955:2012) & rbya$age %in% c(3:15),]
  #rbya <- rbya[rbya$year %in% c(1985:2012) & rbya$age %in% c(3:15),]
  
  rby <- rbx$rby
  #rby <- rby[rby$year %in% c(1955:2012),]
  #rby <- rby[rby$year %in% c(1985:2012),]
  # convert rbyaa to FLQuants
  
  x <- FLQuants()
  for (i in 3:17)
  {
    df <- rbya[,c(1,2,i)]
    nome <- names(rbya[i])
    names(df)[3] <- 'data'
    x[[nome]] <- as.FLQuant(df)
  }
  
  
  x <- FLStock(name = sName, desc = sDesc,
               catch.n = x$catch.n,
               catch.wt = x$catch.wt,
               landings.n = x$catch.n,
               landings.wt = x$catch.wt,
               stock.n=x$stock.n,
               stock.wt = x$stock.wt,
               m = x$m,
               mat = x$mat,
               harvest=x$harvest)
  
  catch(x) <- rby$oY
  discards.n(x) <- 0
  discards.wt(x) <- 0
  discards(x) <- 0
  landings(x) <- rby$oY
  
  if(length(pf) ==  1) pf <- rep(pf,nAge)
  harvest.spwn(x) <- pf
  
  if(length(pm) ==  1) pm <- rep(pm,nAge)
  m.spwn(x) <- pm
  
  #units(x)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4),
  #                            "NA", "NA", "f", "NA", "NA"))
  range(x) <- c(a1,a2,a2,y1,y2,cF1,cF2)
  return(x)
}

#' @title sen from rbya
#' 
#' @description XXX
#' 
#' @author Einar Hjorleifsson
#' 
#' @export
#' 
#' @param x XXX

sen_from_rbya <- function(x) {
  value <- NULL
  # estimating sen
  file <- paste(path.package("fishvise"),"extdata/resultsbyyearandage",sep="/")
  rbya <- read.rbya(file)
  i <- rbya$year %in% c(1985:2012) & rbya$age %in% 3:14
  x <- rbya[i ,c("year","age","tF","wC","wX","xN")]
  x$wC <- x$wC/1e3
  x$wX <- x$wX/1e3
  
  tF <- ddply(x[x$age %in% 5:10,],"year",summarise,refF=mean(tF))
  x <- join(x,tF)
  x$sF <- x$tF/x$refF
  
  d <- melt(x[,c("year","age","sF","wC","wX","xN")],id.vars=c("year","age"))
  d <- ddply(d,c("variable","age"),summarise,ave=mean(value,na.rm=T),cv=sqrt(var(value,na.rm=T))/ave)
  
  nFleets <- 1
  fleet_Fs <- matrix(c(5,10),ncol=1,nrow=2)
  colnames(fleet_Fs) <- "sF"
  fleet_Fs_names <- colnames(fleet_Fs)
  weight_names <- "wC"
  
  ages <- c(3:14)
  sel <- cvsel <- wgt <- cvwgt <- matrix(NA,nrow=length(ages),ncol=nFleets)
  x <- d
  names(x) <- c("id","age","value","cv")
  for (i in 1:nFleets) {
    x1 <- x[x$id %in% fleet_Fs_names[i],]
    x1$value <- x1$value/mean(x1$value[x1$age %in% c(fleet_Fs[1,i]:fleet_Fs[2,i])])
    sel[,i] <- x1[,'value']
    cvsel[,i] <- x1[,'cv']
    x1 <- x[x$id %in% weight_names[i],]
    wgt[,i] <- x1[,"value"]
    cvwgt[,i] <- x1[,"value"]
  }
  bio <- cvbio <- matrix(NA,nrow=length(ages),ncol=3)
  bio[,1]   <- 0.2
  bio[,2]   <- x[x$id %in% 'xN','value']
  bio[,3]   <- x[x$id %in% 'wX','value']
  cvbio[,1] <- 0.0
  cvbio[,2] <- x[x$id %in% 'xN','cv']
  cvbio[,3] <- x[x$id %in% 'wX','cv']
  
  cat_age.dat("age.dat",1,0,0,sel,cvsel,wgt,cvwgt,bio,cvbio) 
}

# Stuff from the surbar package


#' @title Read in Lowestoft-format VPA data
#' 
#' @export
#' @param filename The name of the file to read in
#' @param Format The format of the output, available is "List","Wide","Long"
read.lowestoft <- function(filename, Format="List")
{
  y <- scan(filename, skip = 2, nlines = 1, quiet = TRUE)
  a <- scan(filename, skip = 3, nlines = 1, quiet = TRUE)
  tab <- read.delim(filename, header = FALSE, sep = "", skip = 5)
  names(tab) <- c(a[1]:a[2])
  rownames(tab) <- c(y[1]:y[2])
  if(Format == "List") return(list(y = y, a = a, tab = tab))
  if(Format == "Wide") return(tab)
  tab$year <- rownames(tab)
  tab <- melt(tab,id.vars="year")
  return(tab)
}


#' @title Read in Lowestoft-format survey data
#' 
#' @export
#' @param filename The name of the file to read in
read.lowestoft.survey <- function(filename)
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

