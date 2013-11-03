#' @title Calculate length based indices
#' 
#' @description Calculates abundance and biomass survey indices based on length
#' classes for a particular species in a given year.
#' 
#' @export
#' @param lengths \emph{data.frame} containing length measurement for a given
#' species in a given survey year.
#' @param stations \emph{data.frame} containg station survey station informations.
#' @param sex \emph{boolean} A flag indicating if indices should also be
#' compiled by sex
#' @param lenClass \emph{numerical vector} containing length classes to 
#' compile the indices for
#' @param yr \emph{numerical} value for year
#' @param lwcoeff \emph{numerical vector} of length 2, containing parameter
#' a and b of the length weight relationship.
bioIndex <- function(lengths,stations,sex,lenClass,yr,lwcoeff) {
  for( i in 1:length(lenClass)) {
    L <- lengths
    
    # A.1 total number of fish of a length class by station
    le <- L[L$lengd==lenClass[i],]    
    if(nrow(le) > 0) {
      x <- apply.shrink(le$fj.alls,le$synis.id,sum) 
      names(x) <- c("synis.id","fj")
      st <- gJoin(stations[,c("newstrata","toglengd","synis.id")],x,"synis.id",set=0)
    } else {
      st <- stations[,c("newstrata","toglengd","synis.id")]
      st$fj <- 0
    }    
    # A.2 total number of fish of a length class by station and sex
    if(sex) {
      le <- L[L$lengd==lenClass[i] & L$kyn==1 & !is.na(L$kyn),]
      if(nrow(le) > 0) {
        x <- apply.shrink(le$fj.alls,le$synis.id,sum) 
        names(x) <- c("synis.id","fjhaenga")
        st <- gJoin(st,x,"synis.id",set=0)
      } else {
        st$fjhaenga <- 0
      }
      le <- L[L$lengd==lenClass[i] & L$kyn==2 & !is.na(L$kyn),]
      if(nrow(le) > 0) {
        x <- apply.shrink(le$fj.alls,le$synis.id,sum) 
        names(x) <- c("synis.id","fjhrygna")
        st <- gJoin(st,x,"synis.id",set=0)
      } else {
        st$fjhrygna <- 0
      }
    }

    # B. Biomass GREATER than a certain length class
    le <- L[L$lengd >= lenClass[i],]
    if(nrow(le) > 0) {
      x <- apply.shrink(le$bio,le$synis.id,sum) 
      names(x) <- c("synis.id","bioge") 
      st <- gJoin(st,x,"synis.id",set=0)
    } else {
      st$bioge <- 0
    }
    
    # C. Abundance LESS than a certain length class
    le <- L[L$lengd <= lenClass[i],]
    if(nrow(le) > 0) {
      x <- apply.shrink(le$fj.alls,le$synis.id,sum) 
      names(x) <- c("synis.id","fjle")
      st <- gJoin(st,x,"synis.id",set=0)
    } else {
      st$fjle <- 0 
    }
    
    # D. Calculate the indices
    visit <- Calc.index(st,"fj")
    B0 <- Calc.index(st,"bioge")
    N0 <- Calc.index(st,"fjle")    
    if(sex) {
      N0sex1 <- Calc.index(st,"fjhaenga")
      N0sex2 <- Calc.index(st,"fjhrygna")
    }
    
    base0 <- visit$result[,c("strata","total","cv")]
    names(base0)[2:3] <- c("fj","cv.fj")
    base0$bio.staerri <- B0$result[,"total"]
    base0$cv.bio.staerri <- B0$result[,"cv"]
    base0$fj.minni <- N0$result[,"total"]
    base0$cv.fj.minni <- N0$result[,"cv"]
    
    if(sex) {
      base0$fj.haenga <- N0sex1$result[,"total"]
      base0$cv.fj.haenga <- N0sex1$result[,"cv"]
      base0$fj.hrygna <- N0sex2$result[,"total"]
      base0$cv.fj.hrygna <- N0sex2$result[,"cv"]
    }
    
    aggr0 <- visit$aggr.output[,c("total","cv")]
    names(aggr0) <- c("fj","cv.fj") 
    aggr0$bio.staerri <- B0$aggr.output[,"total"]
    aggr0$cv.bio.staerri <- B0$aggr.output[,"cv"]
    aggr0$fj.minni <- N0$aggr.output[,"total"]
    aggr0$cv.fj.minni <- N0$aggr.output[,"cv"]
    
    if(sex) {
      aggr0$fj.haenga <- N0sex1$aggr.output[,"total"]
      aggr0$cv.fj.haenga <- N0sex1$aggr.output[,"cv"]
      aggr0$fj.hrygna <- N0sex2$aggr.output[,"total"]
      aggr0$cv.fj.hrygna <- N0sex2$aggr.output[,"cv"]
    }
    
    aggr0$svaedi <- dimnames(aggr0)[[1]]
    aggr0$svaedisnr <- 1:nrow(aggr0)
    dimnames(aggr0)[[1]] <- 1:nrow(aggr0)
    base0$lengd <- lenClass[i]
    aggr0$lengd <- lenClass[i]
    base0$ar <- yr
    aggr0$ar <- yr
    
    if(i == 1 ) {
      base <- base0
      aggr <- aggr0
    } else {
      base <- rbind(base,base0)
      aggr <- rbind(aggr,aggr0)
    }
  }
  aggr$bio <- aggr$fj*lwcoeff[1]*aggr$lengd^lwcoeff[2]/1e3
  base$bio <- base$fj*lwcoeff[1]*base$lengd^lwcoeff[2]/1e3
  
  outdata <- list(base=base,aggr=aggr)
  return(outdata)
}

#' Calculate survey indices
#' 
#' Original function from Hoski. This function is used in \code{\link{bioIndex}}.
#' The function does in principle three things:
#' \itemize{
#' \item Standardizes value (e.g. number of fish) by tow length. This is can be
#' done as a separate step by using \code{\link{index.standardize}}.
#' \item Calculates stratified indices. This can be done as a separate step by
#' using \code{\link{index.stratify}}.
#' \item Aggregates the stratified indices in various ways. This can be done as
#' a separate step by using \code{\link{index.aggregate}}.
#' }
#' 
#' @export
#' @param sfile \emph{data.frame} Station table that contains columns 
#' \code{synis.id} and \code{newstrata} for the \code{z}-file. Note that
#' the dataframe does not have to include abundance ....
#' @param colname Column to be used as the basis for the calculation. These
#' and be \code{fj}, ....
#' @param strata.list \emph{list} that contains the stratas to be used as the
#' basis for the calculation. Norally use \code{strata.list=ralllist}.
#' @param std.toglengd \emph{numeric} The length of the standard tow, default
#' is 4 (miles).
#' @param trollbreidd The swept width, default is 17. The area swept of each tow
#' is then calculated as \code{std.area <- (std.toglengd * trollbreidd)/1852}
#' resulting in standardized area swept of the tow in square miles.
#' @param combine.output \emph{list} that contains list of stratas upon which
#' the calculation should be aggregated. Norally (default) use 
#' \code{combine.output=Std.aggregation}.
#' @param use.rallarea \emph{boolean} If TRUE (default) then strata areas 
#' specified in \code{attributes(STRATAS)$rall.area} is used. If FALSE
#' strata areas specified in \code{attributes(STRATAS)$area} is used.
#' @param leidretta.fyrir.toglengd \emph{Boolean} Default TRUE
#' \itemize{
#' \item if \code{toglengd} in \code{sfile} \emph{NA} then set \code{toglengd}
#' to \code{std.toglengd}
#' \item if \code{toglengd} in \code{sfile} is less than \code{mintoglengd}
#' then set \code{toglengd} to \code{mintoglengd}
#' \item if \code{toglengd} in \code{sfile} is greater than \code{maxtoglengd}
#' then set \code{toglengd} to \code{maxtoglengd}
#' }
#' @param z DO NOT UNDERSTAND THIS
#' @param std.cv \emph{numerical} The default coefficient of variation. This
#' value will be used in cases when cv can not be calculated.
#' @param mintoglengd \emph{numerical} Value repaces \code{toglengd} in \code{sfile}
#' if the latter is smaller than stated (and if \code{leidretta.fyrir.toglengd=TRUE}.
#' @param maxtoglengd Value repaces \code{toglengd} in \code{sfile}
#' if the latter is greater than stated (and if \code{leidretta.fyrir.toglengd=TRUE}.
#' @return Returns a \emph{list} with the following \emph{data.frame}s:
#' \itemize{
#' \item \code{$result} with the following columns:
#' \itemize{
#' \item \code{strata}: Names/number of the strata
#' \item \code{mean}: Mean (number or biomass) within the strata
#' \item \code{stdev}: Standard deviation within the strata
#' \item \code{count}: Number of tows within the strata
#' \item \code{area}: The area (sqmiles) of the strata
#' \item \code{se}: Standard error
#' \item \code{cv}: Coefficient of variation
#' \item \code{total}: Total (number or biomass) in the strata.
#' }
#' \item \code{$Res.names} Contains information about the strata names
#' \item \code{$Aggr.output} with the following columns:
#' \itemize{
#' \item \code{mean}: Mean (number or biomass) within an area
#' \item \code{se}: Standard error within an area
#' \item \code{count}: Number of stations within an area ??????
#' \item \code{area}: The area (sqmiles) of the area
#' \item \code{total}: Total (number or biomass) in the area.
#' }
#' }
#' 
Calc.index <- function (sfile, colname, strata.list, std.toglengd = 4, 
                        trollbreidd = 17, combine.output = Std.aggregation,
                        use.rallarea = T, leidretta.fyrir.toglengd = T, z,
                        std.cv = 1, mintoglengd, maxtoglengd) {
  sfile$strata <- sfile$newstrata
  
  if (!missing(z)) {
    sfile$outcome <- z
    colname <- "outcome"
  }
  
  # 1) Standardize values by tow length (see index.standardize)
  if (missing(mintoglengd)) mintoglengd <- std.toglengd/2
  if (missing(maxtoglengd)) maxtoglengd <- std.toglengd * 2
  
  std.area <- std.toglengd * trollbreidd/1852
  
  if (!is.na(match("toglengd", names(sfile))) && leidretta.fyrir.toglengd) 
  {
    i <- is.na(sfile$toglengd)
    if (any(i)) sfile$toglengd[i] <- std.toglengd
    i <- sfile$toglengd < mintoglengd
    if (any(i)) sfile$toglengd[i] <- mintoglengd
    i <- sfile$toglengd > maxtoglengd
    if (any(i)) sfile$toglengd[i] <- maxtoglengd
    sfile[, colname] <- sfile[, colname] * std.toglengd/sfile$toglengd
  }
  
  
  if (use.rallarea) 
  {
    areas <- attributes(STRATAS)$rall.area
  } else {
    areas <- attributes(STRATAS)$area
  }
  
  Names <- attributes(STRATAS)$name
  
  if (!missing(strata.list)) 
  {
    for (i in 1:length(strata.list)) 
    {
      areas[strata.list[[i[1]]]] <- sum(areas[strata.list[[i]]])
      j <- !is.na(match(sfile$strata, strata.list[[i]]))
      if (any(j)) 
        sfile$strata[j] <- strata.list[[i]][1]
    }
  }
  
  tmp6 <- apply.shrink(sfile[, colname], sfile$strata, mean)
  tmp7 <- apply.shrink(sfile[, colname], sfile$strata, sdev)
  tmp8 <- apply.shrink(rep(1, nrow(sfile)), sfile$strata, sum)
  
  result <- data.frame(strata = tmp8[, 1], mean = tmp6[, 2], 
                       sdev = tmp7[, 2], count = tmp8[, 2])
  
  names(result) <- c("strata", "mean", "sdev", "count")
  
  i <- result$count == 1
  if (any(i)) result$sdev[i] <- result$mean[i] * std.cv
  result$area <- areas[result$strata]/1.854^2
  result$se <- result$sdev/sqrt(result$count)
  result$cv <- result$se/result$mean
  result$total <- result$mean * result$area/std.area
  Res.names <- Names[result$strata]
  aggr.output <- data.frame(matrix(0, length(combine.output), 6))
  names(aggr.output) <- c("mean", "se", "cv", "count", "area", "total")
  row.names(aggr.output) <- names(combine.output)
  for (i in 1:length(combine.output)) {
    j <- !is.na(match(result$strata, combine.output[[i]]))
    j1 <- c(1:length(j))
    j1 <- j1[j]
    if (length(j1) > 0) aggr.output[i, ] <- Combine.strata(result, j1)
  }
  
  aggr.output$area <- round(aggr.output$area)
  aggr.output$mean <- round(aggr.output$mean, 3)
  aggr.output$se <- round(aggr.output$se, 3)
  aggr.output$cv <- round(aggr.output$cv, 3)
  aggr.output$se <- round(aggr.output$se, 4)
  aggr.output$total <- round(aggr.output$total, 1)
  result$area <- round(result$area)
  result$mean <- round(result$mean, 3)
  result$sdev <- round(result$sdev, 3)
  result$cv <- round(result$cv, 3)
  result$se <- round(result$se, 4)
  result$total <- round(result$total, 1)
  return(list(result = result, Res.names = Res.names, aggr.output = aggr.output))
}

#' Sums mean, standard error, cv by aggregated area
#' 
#' Function used both in \code{\link{Calc.index}} and \code{\link{index.aggregate}}.
#' 
#' @export
#' @aliases index.aggregate.combine
#' @param result Result
#' @param index Index
Combine.strata <- function (result, index)
{
  if (!missing(index)) result <- result[index, ]
  Mean <- sum(result$mean * result$area)/sum(result$area)
  Sum <- sum(result$mean * result$area)
  total <- sum(result$total)
  totalarea <- sum(result$area)
  i <- !is.na(result$sdev)
  tmpsum <- sum(result$mean[i] * result$area[i])
  Calc.sdev <- sqrt(sum(result$sdev[i]^2 * result$area[i]^2/result$count[i])/sum(result$area[i])^2)
  Sdev <- Calc.sdev * Sum/tmpsum
  return(data.frame(mean = Mean, se = Sdev, cv = Sdev/Mean, 
                    count = sum(result$count), area = totalarea, total = total))
}

#' Standardizes value by tow length
#' 
#' Standaridize value of interest. This is equivalent to the first part of 
#' the function \code{\link{Calc.index}}.
#' 
#' @export
#' @param value The value to standardize (e.g. number of fish)
#' @param towlength the reported towlength
#' @param std.length The tow length to standardize, default 4 nautical miles
#' @param std.width The width swept, default 17 m
#' @param to.area A flag indicating if results should be by area, default FALSE
#' @param min.length Minimum tow length, if missing set to half the standardized tow length
#' @param max.length Maxium tow length, if missing set to twice the standardized tow length
index.standardize <- function (value, towlength, std.length = 4,
                               std.width = 17, to.area=FALSE,min.length, max.length) {
  
  if (missing(min.length)) min.length <- std.length/2
  if (missing(max.length)) max.length <- std.length * 2
  
  i <- is.na(towlength)
  if (any(i)) towlength[i] <- std.length
  i <- towlength < min.length
  if (any(i)) towlength[i] <- min.length   
  i <- towlength > max.length
  if (any(i)) towlength[i] <- max.length  
  if(!to.area) value <- value * std.length / towlength
  if(to.area)  value <- value * std.length / towlength * std.width/1852
  return(value)
}

#' Calculates stratified indices
#' 
#' Calculate indices by strata. If standardization by tow length is desired use
#' first  \code{\link{index.standardize}}.
#' This captures the second part of the function \code{\link{Calc.index}}.
#' 
#' @export
#' @param sfile data.frame with strata and abundance indices of each tow
#' @param col.names character vector containing strata name and value of interest
#' @param std.length tow length
#' @param std.width tow width, std.length * std.width gives area swept 
#' @param use.rallarea Use rall area
#' @param strata.list strata list
#' @param std.cv Standard cv
index.stratify <- function (sfile, col.names=c('strata','v'), std.length = 4,
                            std.width=17,use.rallarea=TRUE, strata.list, std.cv = 1) {
  
  sfile$strata <- sfile[,col.names[1]]
  sfile$v  <-     sfile[,col.names[2]]
  
  # strata area bookkeping, could be turned into a separate function
  if (use.rallarea) {
    areas <- attributes(STRATAS)$rall.area
  } else {
    areas <- attributes(STRATAS)$area
  }
  Names <- attributes(STRATAS)$name
  
  # If some other strata is provide by the user
  if (!missing(strata.list)) {
    for (i in 1:length(strata.list)) {
      areas[strata.list[[i[1]]]] <- sum(areas[strata.list[[i]]])
      j <- !is.na(match(sfile$strata, strata.list[[i]]))
      if (any(j)) 
        sfile$strata[j] <- strata.list[[i]][1]
    }
  }
  
  ####  Finally the actual calculation
  result <- ddply(sfile,c('strata'),summarise,v.count=length(v),v.mean=mean(v),v.sdev=sdev(v))
  i <- result$v.count == 1
  if (any(i)) result$v.sdev[i] <- result$v.mean[i] * std.cv
  result$se <- result$v.sdev/sqrt(result$v.count)
  result$cv <- result$se/result$v.mean
  
  result$area <- areas[result$strata]/1.854^2
  std.area <- std.length * std.width/1852
  result$total <- result$v.mean * result$area/std.area
  names(result) <- c('strata','count','mean','sdev','se','cv','area','total')
  Res.names <- Names[result$strata]
  return(list(result = result, Res.names = Res.names))
}


#' Aggregates stratified indices by users desire
#' 
#' Aggregates stratified indices that have been calculated by \code{\link{index.stratify}}
#' This captures the second part of the function \code{\link{Calc.index}}.
#' 
#' @export
#' @param indices Dataframe containing ...
#' @param combine.output A list with ..., if missing uses Std.aggregation
index.aggregate <- function(indices,combine.output) {
  
  if(missing(combine.output)) combine.output <- Std.aggregation
  # Calculate various aggregations of the index
  agr.indices <- data.frame(matrix(0, length(combine.output), 6))
  names(agr.indices) <- c("mean", "se", "cv", "count", "area", "total")
  row.names(agr.indices) <- names(combine.output)
  for (i in 1:length(combine.output)) {
    j <- !is.na(match(indices$strata, combine.output[[i]]))
    j1 <- c(1:length(j))
    j1 <- j1[j]
    if (length(j1) > 0) agr.indices[i, ] <- Combine.strata(indices, j1)
  }
  
  agr.indices$area <- round(agr.indices$area)
  agr.indices$mean <- round(agr.indices$mean, 3)
  agr.indices$se <- round(agr.indices$se, 3)
  agr.indices$cv <- round(agr.indices$cv, 3)
  agr.indices$se <- round(agr.indices$se, 4)
  agr.indices$total <- round(agr.indices$total, 1)
  
  return(list(indices = indices, agr.indices = agr.indices))
}


