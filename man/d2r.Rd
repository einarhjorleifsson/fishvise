\name{d2r}
\alias{d2r}
\title{Converts lat and lon to statistical squares}
\usage{
d2r(lat, lon = NULL)
}
\arguments{
  \item{lat}{numerical vector or data.frame with columns
  names 'lat' and 'lon'}

  \item{lon,}{if NULL (default), it is assumed that
  \code{lat} is a \code{data.frame} with column names 'lat'
  and 'lon'}
}
\value{
a vector
}
\description{
\code{d2r} ...
}
\author{
Hoski, documented by Einar
}
\seealso{
\code{\link{r2d}} for converting statistical squares to lat
and lon
}

