\docType{data}
\name{smhSTODVAR}
\alias{smhSTODVAR}
\title{Fall survey (SMH) station list}
\format{Each year is stored as a separate \emph{data.frame} in the \emph{list}.}
\source{
  The object is created and maintained by Höskuldur
  Björnsson. The original source is found in
  "/net/hafkaldi/u2/reikn/Splus5/HAUSTRALL/URV/.RData". It
  is copied into this library via: \code{
  attach("/net/hafkaldi/u2/reikn/Splus5/HAUSTRALL/URV/.RData")
  smhSTODVAR <- haustrall
  save(smhSTODVAR,file='data/smhSTODVAR.rda')
  detach("/net/hafkaldi/u2/reikn/Splus5/HAUSTRALL/URV/.RData")}
}
\description{
  A \emph{list} that contains the fall survey station
  information.
}
\author{
  Hoskuldur Bjornsson
}
\keyword{datasets}

