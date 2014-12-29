#' @title read_lowestoft_file
#' 
#' @description bla, bla, ... Modified 
#' \href{https://github.com/flr/FLCore/blob/master/R/io.VPAsuite.R}{FLCore::readVPAFile} function,
#' here it only returns an matrix rather than a FLQuant-object
#' 
#' @export
#' 
#' @param file name of file, normally the index file name
#' @param sep the separator, default is ""
#' @param quiet boolean, default is TRUE


read_lowestoft_file <- function(file, sep = "", quiet = TRUE) {
  
  if (!file.exists(file)){
    if(quiet==TRUE) stop()
    if(quiet!=TRUE) stop(paste("VPA index file", file, "does not exist"))
  }
  
  
  switch (as.character(file.access(file)),
          "0" = info <- read.table(file, colClasses = "character",
                                   header = FALSE, fill = TRUE, skip = 1,
                                   nrows = 4, sep = sep, comment.char='#'),
          "-1" = info <- matrix(rep("0", 8), nrow = 4, ncol = 2))
  misc <- info[1, 1]
  type <- info[1, 2]
  dfor <- info[4, 1]
  # Switch for file type (dfor; e.g. matrix, scalar, vector)
  switch(misc,
         "1" = {range <- scan(file, skip = 2, nlines = 2, sep = sep, comment.char='#',
                              quiet=quiet)
                ages <- range[3:4]
                nages <- ages[2] - ages[1] + 1
                yrs <- range[1:2]
                nyrs <- yrs[2] - yrs[1] + 1
                dms <- list(age=as.character(ages[1]:ages[2]),year=as.character(yrs[1]:yrs[2]))
                switch(dfor,
                       "1" = a. <- matrix(t(read.table(file = file,
                                                       skip = 5,
                                                       nrows = nyrs,
                                                       sep = sep,
                                                       comment.char='#')[, 1:nages]),
                                          nrow=nages,
                                          ncol=nyrs,
                                          dimnames= dms),
                       "2" = a. <- matrix(rep(scan(file,
                                                   skip = 5,
                                                   sep = sep,
                                                   comment.char='#',
                                                   quiet=quiet)[1:nages], nyrs), 
                                          nrow = nages,
                                          ncol = nyrs,
                                          dimnames = dms),
                       "3" = a. <- matrix(rep(scan(file, 
                                                   skip = 5, 
                                                   sep = sep,
                                                   comment.char='#', 
                                                   quiet=quiet)[1], nyrs * nages),
                                          nrow = nages,
                                          ncol = nyrs,
                                          dimnames = dms),
                       "5" = {
                         dms <- list(age="all",year=as.character(yrs[1]:yrs[2]))
                         a. <- matrix(t(read.table(file = file, 
                                                   skip = 5,
                                                   nrows = nyrs, 
                                                   sep = sep)[,1]), 
                                      nrow = 1, 
                                      ncol = nyrs,
                                      dimnames = dms)
                       })
                #needed to go from int to double
                #a. <- as.numeric(a.)
                return(a.)
                },
         "0" = cat("Invalid file. Cannot read file:-", file, "\n"),
         if(quiet != TRUE) cat("Tuning file", file, "not read", "\n")
         )
  }

 




#' @title read_lowestoft2
#' 
#' @description Modified \href{https://github.com/flr/FLCore/blob/master/R/io.VPAsuite.R}{FLCore::readVPA} 
#' function in the FLCore-package, except here it only returns a list or a data.frame
#' 
#' @export
#' 
#' @param file name of file, normally the index file name
#' @param sep the separator, default is ""
#' @param quiet boolean, default is TRUE

read_lowestoft2 <- function(file, sep = "", quiet=TRUE) {
  if (!file.exists(file)){
    if(quiet==TRUE) stop()
    if(quiet!=TRUE) stop(paste("VPA index file", file, "does not exist"))
  }
  dir <- dirname(file)
  files. <- scan(file, what = "character", skip = 2, sep = sep, quiet=quiet)
  for(i in seq(length(files.)))
    if (!grepl(dir,files.[i]))
      files.[i] <- file.path(dir, files.[i], fsep = .Platform$file.sep)
  range1 <- scan(files.[1], skip = 2, nlines = 1, sep = sep, quiet=quiet)
  range2 <- scan(files.[1], skip = 3, nlines = 1, sep = sep, quiet=quiet)
  range <- c(range1[1:2],range2[1:2])
  ages <- range[3:4]
  yrs <- range[1:2]
  #FLStock. <- FLStock(catch.n=FLQuant(NA, dimnames = list(age = ages[1]:ages[2], year = yrs[1]:yrs[2], unit = "unique", season = "all", area = "unique")))
  FLStock. <- list()
  for (i in files.) {
    if (!file.exists(i)){
      if(quiet != TRUE) cat("File ", i, "does not exist", "\n")
    }
    if (file.exists(i)) {
      a. <- read_lowestoft_file(i, sep=sep, quiet=quiet)
      switch(as.character(scan(i, skip = 1, nlines = 1, sep = sep, comment.char='#', quiet=TRUE)[2]),
             "1" = FLStock.$landings <-a.,
             "2" = FLStock.$landings.n <-a.,
             "3" = FLStock.$landings.wt <-a.,
             "4" = FLStock.$stock.wt <-a.,
             "5" = FLStock.$m <-a.,
             "6" = FLStock.$mat <-a.,
             "7" = FLStock.$harvest.spwn<-a.,
             "8" = FLStock.$m.spwn <-a.,
             "21"= FLStock.$discards <-a.,
             "22"= FLStock.$discards.n <-a.,
             "23"= FLStock.$discards.wt <-a.,
             "24"= FLStock.$catch <-a.,
             "25"= FLStock.$catch.n <-a.,
             "26"= FLStock.$catch.wt <-a.,
             "27"= FLStock.$harvest <-a.,
             "28"= FLStock.$stock.n <-a. )
    }
  }
  FLStock.$range <- c(min = ages[1], max = ages[2],
                      plusgroup = ages[2], minyear = yrs[1], maxyear = yrs[2])
  FLStock.$desc <- paste("Imported from a VPA file (",
                         file, "). ", date(), sep="")
  FLStock.$name <- scan(file, nlines = 1, what = character(0),
                        sep = "\n", quiet=TRUE)
  return(FLStock.)
}