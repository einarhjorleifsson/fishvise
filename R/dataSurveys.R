#' @name smbSTODVAR
#' @title Spring survey (SMB) station list
#' @description A \emph{list} that contains the spring survey station information.
#' @docType data
#' @usage Some usage
#' @format Each year is stored as a separate \emph{data.frame} in the \emph{list}.
#' @source
#' The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach("/net/hafkaldi/u2/reikn/Splus5/SMB/.RData")
#' smbSTODVAR <- STODVAR
#' save('smbSTODVAR',file='data/smbSTODVAR.rda')
#' detach("file:/net/hafkaldi/u2/reikn/Splus5/SMB/.RData")}
#' @author Hoskuldur Bjornsson
NULL

#' @name smhSTODVAR
#' @title Fall survey (SMH) station list
#' @description A \emph{list} that contains the fall survey station information.
#' @docType data
#' @usage Some usage
#' @format Each year is stored as a separate \emph{data.frame} in the \emph{list}.
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/HAUSTRALL/URV/.RData".
#' It is copied into this library via:
#' \code{
#' attach("/net/hafkaldi/u2/reikn/Splus5/HAUSTRALL/URV/.RData")
#' smhSTODVAR <- haustrall
#' save(smhSTODVAR,file='data/smhSTODVAR.rda')
#' detach("/net/hafkaldi/u2/reikn/Splus5/HAUSTRALL/URV/.RData")}
#' @author Hoskuldur Bjornsson
NULL

#' @name LWCOEFF
#' @title Length weigth coefficients
#' @description A \emph{list} that contains length-weight coefficients for number of species.
#' @docType data
#' @usage Some usage
#' @format Each element of the list contains parameter a and b for different
#' species. The name of the element of the list corresponds to numerical species
#' code.
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach('/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')
#' save('LWCOEFF',file='data/LWCOEFF.rda')
#' detach('file:/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')}
#' @author Hoskuldur Bjornsson
NULL


#' @name Std.aggregation
#' @title Some title
#' @description from SMB RData
#' @docType data
#' @usage Some usage
#' @format Some format
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach("/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")
#' names(Std.aggregation) <- iconv(names(Std.aggregation),'ISO8859-1','UTF-8')
#' save(Std.aggregation,file='data/Std.aggregation.rda')
#' detach('file:/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')}
#' @author Some author
NULL

#' @name STRATAS
#' @title Some title
#' @description from SMB RData
#' @docType data
#' @usage Some usage
#' @format Some format
#' @source The object is created and maintained by Höskuldur Björnsson. The
#' original source is found in "/net/hafkaldi/u2/reikn/Splus5/SMB/.RData". It
#' is copied into this library via:
#' \code{
#' attach("/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")
#' for (i in 1:length(STRATAS)) {
#'   attributes(STRATAS[[i]])$name <- iconv(attributes(STRATAS[[i]])$name,
#'                                          'ISO8859-1','UTF-8')}
#' save(STRATAS,file='data/STRATAS.rda')
#' detach("file:/net/hafkaldi/u2/reikn/Splus5/SMB/GEOMETRY.NEW/.RData")}
#' @author Some author
NULL

#' @name LENGDIR
#' @title Some title
#' @description from SMB RData
#' @docType data
#' @usage Some usage
#' @format Some format
#' @source
#' \code{
#' attach('/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')
#' save('LENGDIR',file='data/LENGDIR.rda')
#' detach('file:/net/hafkaldi/u2/reikn/Splus5/SMB/.RData')}
#' @author Some author
NULL