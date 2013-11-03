\docType{data}
\name{smbSTODVAR}
\alias{smbSTODVAR}
\title{Spring survey (SMB) station list}
\format{Each year is stored as a separate \emph{data.frame} in the \emph{list}.}
\source{
  The object is created and maintained by Höskuldur
  Björnsson. The original source is found in
  "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It is copied
  into this library via: \code{
  attach("/net/hafkaldi/u2/reikn/Splus5/SMB/.RData")
  smbSTODVAR <- STODVAR
  save('smbSTODVAR',file='data/smbSTODVAR.rda')
  detach("file:/net/hafkaldi/u2/reikn/Splus5/SMB/.RData")}
}
\description{
  A \emph{list} that contains the spring survey station
  information.
}
\author{
  Hoskuldur Bjornsson
}
\keyword{datasets}

