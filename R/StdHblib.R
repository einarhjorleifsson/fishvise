#attach("/home/einarhj/stasi/StdHblib/.RData")

#' @title Rattach
#' 
#' @description Modified \code{attach}. If in search path is, unless otherwise
#' specied put in search path 2.
#' 
#' @export
#' 
#' @param what The directory containing .RData
#' @param pos integer specifying position in search() where to attach.
#'            If not specified, will attached to search path 2.
#' @param ... other arguments to pass to \code{attach}
Rattach <- function (what, pos, ...) 
{
  if (file_test("-d", what)) 
    what <- paste(what, ".RData", sep = "/")
  txt <- paste("file:", what, sep = "")
  if (!is.na(match(txt, search()))) {
    pos1 <- match(txt, search())
    detach(pos = pos1)
    if (missing(pos)) 
      attach(what, pos = pos1, ...)
    else attach(what, pos = pos, ...)
  }
  else {
    attach(what, ...)
  }
}

#' @title residplot
#' 
#' @description A bubble plot of residuals
#' 
#' @export
#' 
#' @author Hoskuldur Bjornsson
#' 
#' @param residuals XXX
#' @param xpos XXX
#' @param ypos XXX
#' @param maxsize XXX
#' @param poscol XXX
#' @param linecol XXX
#' @param lwd XXX
#' @param n XXX
#' @param maxn XXX
#' @param negcol XXX
#' @param txt XXX
#' @param cex XXX
#' @param xlab XXX
#' @param ylab XXX
#' @param axes XXX
#' @param arg XXX
#' @param argcol XXX
#' @param arglty XXX
#' @param cn XXX
#' @param append XXX
#' 
residplot <- function (residuals, xpos, ypos, maxsize = 0.2, poscol = 2, linecol = 1, 
          lwd = 1, n = 50, maxn, negcol, txt = F, cex = 0.5, xlab = "", 
          ylab = "", axes = T, arg = T, argcol = 20, arglty = 2, cn = c("x", 
                                                                        "y", "z"), append = F) 
{
  par(err = -1)
  if (is.data.frame(residuals)) {
    x <- residuals[, cn[1]]
    y <- residuals[, cn[2]]
    residuals <- residuals[, cn[3]]
  }
  else {
    residuals <- t(residuals)
    if (missing(maxn)) 
      maxn <- max(abs(residuals), na.rm = T)
    if (missing(xpos)) 
      xpos <- 1:nrow(residuals)
    if (missing(ypos)) 
      ypos <- 1:ncol(residuals)
    x <- matrix(xpos, length(xpos), length(ypos))
    y <- matrix(ypos, length(xpos), length(ypos), byrow = T)
  }
  if (!append) 
    plot(x, y, type = "n", xlab = xlab, ylab = ylab, axes = axes)
  x.bck <- x
  y.bck <- y
  if (arg) {
    r <- x.bck - y.bck
    tmp <- unique(r)
    for (i in 1:length(tmp)) {
      j <- r == tmp[i]
      lines(x.bck[j], y.bck[j], col = argcol, lty = arglty)
    }
  }
  plt <- par()$pin
  xscale <- (par()$usr[2] - par()$usr[1])/plt[1] * maxsize
  yscale <- (par()$usr[4] - par()$usr[3])/plt[2] * maxsize
  rx <- c(unlist(sqrt(abs(residuals)/maxn) * xscale))
  ry <- c(unlist(sqrt(abs(residuals)/maxn) * yscale))
  theta <- seq(0, 2 * pi, length = n)
  n1 <- length(rx)
  theta <- matrix(theta, n1, n, byrow = T)
  x <- matrix(x, n1, n)
  y <- matrix(y, n1, n)
  rx <- matrix(rx, n1, n)
  ry <- matrix(ry, n1, n)
  x <- x + rx * cos(theta)
  y <- y + ry * sin(theta)
  x <- cbind(x, rep(NA, nrow(x)))
  y <- cbind(y, rep(NA, nrow(y)))
  i <- residuals > 0
  if (any(i)) {
    polygon(c(t(x[i, ])), c(t(y[i, ])), col = poscol)
    lines(c(t(x[i, ])), c(t(y[i, ])), col = linecol, lwd = lwd)
  }
  i <- residuals < 0
  if (any(i)) {
    if (!missing(negcol)) 
      polygon(c(t(x[i, ])), c(t(y[i, ])), col = negcol)
    lines(c(t(x[i, ])), c(t(y[i, ])), col = linecol, lwd = lwd)
  }
  if (txt) 
    text(x.bck, y.bck, as.character(round(residuals)), cex = cex)
  return(invisible())
}

