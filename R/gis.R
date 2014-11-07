#' @title trim_shape_df
#' 
#' @description Trims shape data.frames and returns a shape data.frame
#' 
#' @export
#' 
#' @param x A shape data frame
#' @param xlim Longitudinal boundaries
#' @param ylim Latitudinal boundaries
trim_shape_df <- function(x,xlim=c(-30,10),ylim=c(64,67)) {
  x$.id <- 1:nrow(x)
  txt <- paste("POLYGON((",
               xlim[1],ylim[1],",",
               xlim[2],ylim[1],",",
               xlim[2],ylim[2],",",
               xlim[1],ylim[2],",",
               xlim[1],ylim[1],"))")
  bbx <- rgeos::readWKT(txt)
  sp::proj4string(bbx) <- sp::proj4string(x)
  #x.cut <- gIntersection(x, bbx)
  i <- rgeos::gIntersects(x,bbx,byid=TRUE)
  d <- x@data[i,]
  x <- x[x$.id %in% d$.id,]
}