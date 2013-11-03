\docType{data}
\name{STRATAS}
\alias{STRATAS}
\title{Some title}
\format{Some format}
\source{
  The object is created and maintained by Höskuldur
  Björnsson. The original source is found in
  "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It is copied
  into this library via: \code{
  attach("/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")
  for (i in 1:length(STRATAS)) {
  attributes(STRATAS[[i]])$name <-
  iconv(attributes(STRATAS[[i]])$name,
  'ISO8859-1','UTF-8')}
  save(STRATAS,file='data/STRATAS.rda')
  detach("file:/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")}
}
\description{
  from SMB RData
}
\author{
  Some author
}
\keyword{datasets}

