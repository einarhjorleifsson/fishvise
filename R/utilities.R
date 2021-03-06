#' @title grep2
#' 
#' @description List files that contain a particular text string
#' 
#' @export
#' 
#' @param directory Name of the directory
#' @param pattern The string to search for

grep2 <- function(directory,pattern) {
  cmd <- paste("grep -rnw",directory,"-e",pattern)
  system(cmd)
}

#' @title find_text
#' 
#' @description finds text in files in a directory
#' 
#' @export
#' 
#' @param directory Name of the directory
#' @param pattern The string to search for

find_text <- function(pattern,file,directory) {
  if(!missing(directory)) {
    file <- paste(directory,file,sep="/")
  }
  cm <- paste("grep -1",pattern,file)
  system(cm)
}




#' @title Trim 'tabs' from start and end of string
#' 
#' @description Function is enspired by str_trim function in the stringr package. 
#' The str_trim function is for trimming whitespace, here tabs are trimmed.
#' @param string input character vector
#' @param side side on which character string is removed (left, right or both)
#' @export 

str_trim_tab <- function(string,side='both') {
  string <- stringr:::check_string(string)
  stopifnot(length(side) == 1)
  side <- match.arg(side, c("left", "right", "both"))
  pattern <- switch(side, left = "^\\t+", right = "\\t+$", 
                    both = "^\\t+|\\t+$")
  stringr::str_replace_all(string, pattern, "")
}

#' @title Trim 'commas' from start and end of string
#' 
#' @description Function is enspired by str_trim function in the stringr package. 
#' The str_trim function is for trimming whitespace, here commmas are trimmed.
#' @param string input character vector
#' @param side side on which character string is removed (left, right or both)
#' @export

str_trim_commas <- function(string,side='both') {
  string <- stringr:::check_string(string)
  stopifnot(length(side) == 1)
  side <- match.arg(side, c("left", "right", "both"))
  pattern <- switch(side, left = "^,+", right = ",+$", 
                    both = "^,+|,t+$")
  stringr::str_replace_all(string, pattern, "")
}

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

#' sdev
#' 
#' Calculates standard deviation from variance
#' 
#' @param x Value
sdev <- function (x) return(sqrt(var(x)))


#' @title Align stock and recruitment data
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param data data.frame that contains in its first three rows year, recruitment
#' and ssb.
#' @param col.names vector that contains the names for year, recruitment and ssb.
#' @param aR integer that contains the age of recruits
#' 
align_ssb_r <- function(data,col.names=c("year","r","ssb"),aR) {
  x <- data[,col.names]
  data$r <- c(x$r[(aR+1):nrow(x)],rep(NA,aR))
  return(data)
}

#' @title Calculate quantiles
#' 
#' @description xxx
#' 
#' @param d xx
#' @param d.det xx
#' @param variable xx
#' @export

calc.quantiles <- function(d, d.det, variable="variable") {
  
  q05 <- q10 <- q16 <- q50 <- q84 <- q90 <- q95 <- summarise <- value <- NULL
  x <- plyr::ddply(d,c("variable"),summarise,q05=quantile(value,0.05),q10=quantile(value,0.10),q16=quantile(value,0.16),q50=quantile(value,0.50),q84=quantile(value,0.84),q90=quantile(value,0.90),q95=quantile(value,0.95))
  if(!missing(d.det)) x$mean <- d.det$value
  
  return(x)
}

#' @title Calculate quantiles
#' 
#' @description XXX
#' 
#' @export 
#' 
#' @param x XXX
#' @param ... additional stuff
i90 <- function(x, ...) {
  qs <- quantile(as.numeric(x), probs = c(0.05, 0.95), na.rm = TRUE)
  names(qs) <- c("ymin","ymax")
  return(qs)
}

#' @title Calculate quantiles
#' 
#' @description XXX
#' 
#' @export 
#' 
#' @param x XXX
#' @param ... additional stuff
i80 <- function(x, ...) {
  qs <- quantile(as.numeric(x), probs = c(0.10, 0.90), na.rm = TRUE)
  names(qs) <- c("ymin","ymax")
  return(qs)
}

#' @title Calculate quantiles
#' 
#' @description XXX
#' 
#' @export 
#' 
#' @param x XXX
#' @param ... additional stuff
i50 <- function(x, ...) {
  qs <- quantile(as.numeric(x), probs = c(0.25, 0.75), na.rm = TRUE)
  names(qs) <- c("ymin","ymax")
  return(qs)
}

#' @title Converts character columns in a \code{data.frame} between Encodings
#' 
#' Does the same as \code{iconv} but on a data.frame instead of a vector
#'  
#' @export
#' 
#' @param data data.frame to convert
#' @param from A character string describing the current encoding, default is "ISO8859-1"
#' @param to A character string describing the target encoding
#' @param ... Other things to pass to \code{iconv}
#' @return data.frame
#' @author Einar
#' @seealso \code{\link{iconv}}
iconvDF <- function(data,from="ISO8859-1",to="UTF-8", ...) {
  col_class <- sapply(data,class) == "character"
  for (i in 1:length(col_class)) {
    if(col_class[i] == TRUE) data[,i] <- iconv(data[,i],"ISO8859-1", "UTF-8")
  }
  return(data)
}


#' @title Converts date to correponding quota year
#' 
#' @description XXX
#'
#' @export
#' 
#' 
#' @param x A vector containing dates

date_2_qyear <- function(x) 
{
  res <- ifelse(lubridate::month(x) < 9,
                res <- paste(lubridate::year(x)-1,substr(lubridate::year(x),3,4),sep="/"),
                res <- paste(lubridate::year(x),substr(lubridate::year(x)+1,3,4),sep="/"))
  i <- res == "1990/91"
  res[i] <- "1991/91"
  return(res)
}

#' @title Converts date to first date (1. september) of each quota year
#' 
#' @description Note that the first quota year was from 1. januar 1991 to
#' 31. august 1991. Taken care of in the function.
#'
#' @export
#' 
#' @param x A vector containing dates

date_2_qyear_start <- function(x) 
{
  i <- lubridate::month(x) < 9
  lubridate::year(x[i]) <- lubridate::year(x[i]) - lubridate::years(1)
  lubridate::day(x) <- 1
  lubridate::month(x) <- 9
  
  # fix the first qyear
  i <- lubridate::year(x) == 1991
  lubridate::month(x[i]) <- 1
  
  return(x)
}
