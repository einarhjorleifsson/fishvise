#' @title Convert rvk-format to FLStock object
#' 
#' @description Converts the rby and rbya to FLStock object.
#' 
#' @export
#' 
#' @param rbx A list with two data.frames named rby and rbya.
#' @param sName Character vector containing name of stock
#' @param sDesc Character vectro containing some description
#' @param pf Partial fishing mortality
#' @param pm Partial natural mortality
#' @param aF1 Lower reference age
#' @param aF2 Upper reference age
#' 
#' @note TO DO: check the unit stuff
#' 
rvk2FLStock <- function(rbx, sName="nn", sDesc="none",pf=0,pm=0,aF1=5,aF2=10) {
  
  rbya <- rbx$rbya
  
  # TO DO: Generate standard name converter and use that here
  names(rbya) <- c("year","age","catch.n","catch.wt","stock.wt","stock1.wt",
                   "mat","stock.n","z","harvest","m","Chat","rC","Uobs","Uhat",
                   "Ures","U2obs","U2hat","U2res","run","model","assYear")
  
  a1 <- min(rbya$age)
  a2 <- max(rbya$age)
  nAge <- length(a1:a2)
  
  y1 <- min(rbya$year)
  y2 <- max(rbya$year)
  
  rbya$catch.wt <- ifelse(rbya$catch.wt== -1,0,rbya$catch.wt)
  rbya$stock.wt <- ifelse(rbya$stock.wt== -1,0,rbya$stock.wt)
  #rbya$catch.n  <- ifelse(rbya$catch.n==  -1,0,rbya$catch.n)
  rbya$harvest  <- ifelse(rbya$harvest==  -1,0,rbya$harvest)
  #rbya$m <- ifelse(rbya$age %in% c(1,2),0.0001,rbya$m)
  #rbya$mat <- ifelse(rbya$age %in% c(1,2) | is.na(rbya$mat),0,rbya$mat)
  #rbya <- rbya[rbya$year %in% c(1955:2012) & rbya$age %in% c(3:15),]
  #rbya <- rbya[rbya$year %in% c(1985:2012) & rbya$age %in% c(3:15),]
  
  rby <- rbx$rby
  #rby <- rby[rby$year %in% c(1955:2012),]
  #rby <- rby[rby$year %in% c(1985:2012),]
  
  # convert rbya to FLQuants
  x <- FLQuants()
  for (i in 3:17)
  {
    df <- rbya[,c(1,2,i)]
    nome <- names(rbya[i])
    names(df)[3] <- 'data'
    x[[nome]] <- as.FLQuant(df)
  }
  
  
  x <- FLStock(name = sName, desc = sDesc,
               catch.n = x$catch.n,
               catch.wt = x$catch.wt,
               landings.n = x$catch.n,
               landings.wt = x$catch.wt,
               stock.n=x$stock.n,
               stock.wt = x$stock.wt,
               m = x$m,
               mat = x$mat,
               harvest=x$harvest)
  
  catch(x) <- rby$oY
  discards.n(x) <- 0
  discards.wt(x) <- 0
  discards(x) <- 0
  landings(x) <- rby$oY
  
  if(length(pf) ==  1) pf <- rep(pf,nAge)
  harvest.spwn(x) <- pf
  
  if(length(pm) ==  1) pm <- rep(pm,nAge)
  m.spwn(x) <- pm
  
  #units(x)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4),
  #                            "NA", "NA", "f", "NA", "NA"))
  range(x) <- c(a1,a2,a2,y1,y2,aF1,aF2)
  return(x)
}


#' @title Convert FLStock (or FLSAM) object long data.frame
#' 
#' @description Converts FLStock or FLSAM object to rbya (Results By Year and Age) format.
#' 
#' @export
#' 
#' @param x FLStock or FLSAM object
#' 
#' @return A data.frame containing the fields: year, age, 
#' n (stock in numbers),
#' f (fishing mortality),
#' oC (observed catch),
#' cW (catch weights),
#' ssbW (spawning stock weights),
#' oD (observed dicards),
#' dW (discard weights),
#' oL (observed landings),
#' lW (landings weights),
#' mat (maturity),
#' pF (partial fishing mortality before spawning)
#' pM (partial natural mortality before spawning),
#' m (natural mortality)
#' and f (fishing mortality).
#' 
#' @note TO DO: FLSAM only contains the stock in numbers and fishing mortality by
#' year and age(?????). Need to "adjust" the function to accomodate that.
#' 
FLStock2rbya <- function(x) 
  {
  y <- reshape2::melt(stock.n(x),value.name = "n")[,c("year","age","n")]
  y$f <- reshape2::melt(harvest(x))[,c("value")]
  # if(class(x) != "FLSAM") {  # This may be needed
  y$oC <- reshape2::melt(catch.n(x))[,c("value")]
  y$cW <- reshape2::melt(catch.wt(x))[,c("value")]
  y$ssbW <- reshape2::melt(stock.wt(x))[,c("value")]
  y$oD  = reshape2::melt(discards.n(x))[,c("value")]
  y$dW  = reshape2::melt(discards.wt(x))[,c("value")]
  y$oL  = reshape2::melt(landings.n(x))[,c("value")]
  y$lW  = reshape2::melt(landings.wt(x))[,c("value")]
  y$mat = reshape2::melt(mat(x))[,c("value")]
  y$pF  = reshape2::melt(harvest.spwn(x))[,c("value")]
  y$pM  = reshape2::melt(m.spwn(x))[,c("value")]
  y$m   = reshape2::melt(m(x))[,c("value")]
  # } # END: This may be needed
  return(y)
}