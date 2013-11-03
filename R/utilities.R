#' apply.shrink
#' 
#' local copy of an old faithful from package geo
#' 
#' @param X x
#' @param INDICES indices
#' @param FUN fun
#' @param names Names
#' @param ... additionalThings
apply.shrink <- 
  function (X, INDICES, FUN = NULL, names, ...) 
  {
    if (missing(FUN)) 
      stop("No function to apply to data given (missing argument FUN)")
    if (!is.list(INDICES)) 
      INDICES <- list(INDICES)
    len.data <- length(X)
    all.indices <- rep(0, len.data)
    for (i in rev(INDICES)) {
      if (length(i) != len.data) 
        stop("Data and all indices must have same length")
      i <- as.factor(i)
      all.indices <- all.indices * length(levels(i)) + (as.vector(unclass(i)) - 
                                                          1)
    }
    all.indices <- all.indices + 1
    INDICES <- as.data.frame(INDICES)
    INDICES <- INDICES[match(sort(unique(all.indices)), all.indices, 
                             nomatch = 0), ]
    if (is.character(FUN)) 
      FUN <- getFunction(FUN)
    else if (mode(FUN) != "function") {
      farg <- substitute(FUN)
      if (mode(farg) == "name") 
        FUN <- getFunction(farg)
      else stop(paste("\"", farg, "\" is not a function", sep = ""))
    }
    X <- split(X, all.indices)
    X.apply <- lapply(X, FUN, ...)
    numb.FUN.value <- length(X.apply[[1]])
    if (numb.FUN.value == 1) 
      X.apply <- data.frame(X = unlist(X.apply))
    else X.apply <- data.frame(matrix(unlist(X.apply), ncol = numb.FUN.value, 
                                      byrow = T, dimnames = list(NULL, names(X.apply[[1]]))))
    X.apply <- cbind(INDICES, X.apply)
    if (!missing(names)) 
      names(X.apply) <- names
    return(X.apply)
  }

#' gJoin
#' 
#' The old join from geo, renamed to avoid confusion with join from plyr
#' 
#' @param x X
#' @param y Y
#' @param column Column
#' @param name.x nameX
#' @param name.y nameY
#' @param outer.join outerJoin
#' @param set Set
gJoin <-
  function (x, y, column, name.x, name.y, outer.join = T, set = NA) 
  {
    if (!missing(name.x)) {
      if (!missing(column)) 
        name.x <- unique(c(name.x, column))
      x <- x[, name.x]
    }
    if (!missing(name.y)) {
      if (!missing(column)) 
        name.y <- unique(c(name.y, column))
      y <- y[, name.y]
    }
    if (missing(column)) {
      d1 <- row.names(x)
      d2 <- row.names(y)
    }
    else {
      d1 <- x[, column]
      ind <- match(names(y), column)
      ind <- ind[!is.na(ind)]
      if (length(ind) == 0) 
        d2 <- row.names(y)
      else d2 <- y[, column]
    }
    if (outer.join) {
      y0 <- matrix(set, nrow(x), ncol(y))
      y1 <- as.data.frame(y0)
      for (i in 1:ncol(y0)) y1[, i] <- I(y0[, i])
      names(y1) <- names(y)
      ind <- match(d1, d2)
      index <- c(1:length(ind))
      index <- index[!is.na(ind)]
      ind <- ind[index]
      y1[index, ] <- y[ind, ]
      outcome <- cbind(x, y1)
    }
    else {
      ind <- match(d1, d2)
      ind1 <- c(1:length(ind))
      ind1 <- ind1[!is.na(ind)]
      ind <- ind[!is.na(ind)]
      x <- (x[ind1, ])
      y <- (y[ind, ])
      outcome <- cbind(x, y)
    }
    if (!missing(column)) {
      i <- match(names(outcome), paste(column, "1", sep = ""))
      i1 <- c(1:length(i))
      i1 <- i1[!is.na(i)]
      if (length(i1) > 0) {
        outcome <- outcome[, -i1]
      }
      i <- match(names(outcome), column)
      i1 <- c(1:length(i))
      i1 <- i1[!is.na(i)]
      if (length(i1) > 1) {
        i1 <- i1[-1]
        outcome <- outcome[, -i1]
      }
    }
    return(outcome)
  }

