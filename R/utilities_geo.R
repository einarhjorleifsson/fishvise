#' Converts statistical squares to degrees
#' 
#' \code{r2d} converts statistical rectangles to degrees
#'  
#' @export
#' @param square input vector, numerical
#' @return a list with lat and lon
#' @author Hoski, documented by Einar

#' @seealso \code{\link{sr2d}} for converting 'small' statistical squares
r2d <- function (square) 
{
  lat <- floor(square/100)
  lon <- (square - lat * 100)%%50
  halfb <- (square - 100 * lat - lon)/100
  lon <- -(lon + 0.5)
  lat <- lat + 60 + halfb + 0.25
  return(list(lat = lat, lon = lon))
}

#' Converts small statistical squares to degrees
#' 
#' \code{sr2d} converts small statistical rectangles to degrees
#'
#' @export
#' @param ssquare input vector, numerical
#' @return a list with lat and lon
#' @author Hoski, documented by Einar

#' @seealso \code{\link{r2d}} for converting statistical squares to lat and lon
sr2d <- function (ssquare) 
{
  square <- floor(ssquare/10)
  ssquare <- ssquare - square * 10
  lat <- floor(square/100)
  lon <- (square - lat * 100)%%50
  halfb <- (square - 100 * lat - lon)/100
  lon <- -(lon + 0.5)
  lat <- lat + 60 + halfb + 0.25
  l1.lat <- c(0, 0.125, 0.125, -0.125, -0.125)
  l1.lon <- c(0, -0.25, 0.25, -0.25, 0.25)
  lat <- lat + l1.lat[ssquare + 1]
  lon <- lon + l1.lon[ssquare + 1]
  return(list(lat = lat, lon = lon))
}



#' Converts lat and lon to statistical squares
#' 
#' \code{d2r} ...
#'  
#' @export
#' @param lat numerical vector or data.frame with columns names 'lat' and 'lon'
#' @param lon, if NULL (default), it is assumed that \code{lat} is a \code{data.frame} with column names 'lat' and 'lon'
#' @return a vector
#' @author Hoski, documented by Einar
#' @seealso \code{\link{r2d}} for converting statistical squares to lat and lon
d2r <- function (lat, lon = NULL) 
{
  if (is.null(lon)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  lat <- lat + 1e-06
  lon <- lon - 1e-06
  lon <- -lon
  reit <- (floor(lat) - 60) * 100 + floor(lon)
  reit <- ifelse(lat - floor(lat) > 0.5, reit + 50, reit)
  return(reit)
}


