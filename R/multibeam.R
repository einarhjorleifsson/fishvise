#' @title SQL function for reading SQLite database
#' 
#' @description A little helper function for reading the mb-database
#' 
#' 
#' @param query The sql query
#' @param dbname The SQLite database to connect to
#' 
mb_sql <- function(query, dbname="/net/hafkaldi/u1/mb/prj/2013_11topovise/dbase/mb.sqlite") {
  require(RSQLite)
  driver <- dbDriver("SQLite")
  connect <- dbConnect(driver, dbname=dbname);
  closeup <- function(){
    sqliteCloseConnection(connect)
    sqliteCloseDriver(driver)
  }
  dd <- tryCatch(dbGetQuery(connect, query), finally=closeup)
  #dbDisconnect(connect)
  return(dd)
}



#' @title Gridding data using nearneighbor gmt function
#' 
#' @description XXX
#' 
#' 
#' @param data The xyz-data
#' @param square The square to limit the gridding to
#' @param out.file Name of outputted grid file
#' @param I The XXX. Default value 0.064
#' @param S The XXX. Default value 0.128
#' @param debug A boolean flag. if TRUE returns the generated command without 
#' running the code. If TRUE, runs gmt-nearneighbor.

mb_gridGMT <- function(data,square,out.file,I=0.064,S=0.128,debug=F) {
  if(missing(out.file)) 
    out.file <- paste('s',square,'_',I,'_',S,'.nc',sep='')
  xy=sr2lim(square)
  cmd=paste('nearneighbor ',
            '-R',xy[1],'/',xy[2],'/',xy[3],'/',xy[4],' ',
            '-I',I,'k ',
            '-S',S,'k ',
            '-G',out.file,' ',
            '-V ',data, sep='')
  if(debug) return(cmd)
  system(cmd)
}

#' @title Set the limits for small statistical squares
#' 
#' @description May not be of any bloody use
#' 
#' @param sr The small statistical square

sr2lim <- function(sr) {
  x.min <- as.numeric(sr2d(sr)[2] - 0.25)
  x.max <- as.numeric(sr2d(sr)[2] + 0.25)
  y.min <- as.numeric(sr2d(sr)[1] - 0.125)
  y.max <- as.numeric(sr2d(sr)[1] + 0.125)
  return(c(x.min,x.max,y.min,y.max))
}