#' @title Initial HCR objects
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param ctr Control list

hcr_set_dimensions <- function(ctr) {
  
  
  d <- list()
  
  HRATE <- ctr$HRATE
  
  nR <- ctr$nR
  
  a1 <- ctr$a1
  a2 <- ctr$a2
  n_ages <- a2 - a1 + 1
  y1 <- ctr$y1
  y2 <- ctr$y2
  n_years <- y2 - y1 + 1

  iter <- ctr$iter
  
  # array with age as a dimention
  x <- array(-1,dim=c(n_ages,n_years,length(HRATE),iter),
             dimnames=list(age=a1:a2,
                           year=y1:y2,
                           hrate=HRATE,
                           iter=1:iter))
  d$N <- d$tF <- d$M <- d$pM <- d$pF <- d$selF <- d$selD <- d$selB <- d$C <- d$cW  <- 
    d$sW <- d$cvcW <- d$cvsW <- d$mat  <- x
  
  # arrays without an age dimention
  x <- array(-1,dim=c(n_years,length(HRATE),iter),
             dimnames=list(year=y1:y2,
                           hrate=HRATE,
                           iter=1:iter))
  d$TAC <- d$assError <- x
  
  # array for recruit is based on year classes
  startYC <- y1-nR     # most recent year class with measurement
  nYC <- y2-startYC+1    # number of year classes that need to be
  # simulated
  x <- array(-1,dim=c(nYC,length(HRATE),iter),
             dimnames=list(yearclass=startYC:y2,
                           hrate=HRATE,
                           iter=1:iter))
  d$cvR <- d$rDEV <- d$R <- x
  return(d)
}

#' @title HCR: Setup of assessment errors
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param d XXX
#' @param ctr XXX
#' 
hcr_set_assErrors <- function(d,ctr) 
  {

  n_years  <- dim(d)[1]
  n_hrates <- dim(d)[2]
  n_iters  <- dim(d)[3]

  x <- array(rnorm(n_years + 1000 * n_iters),
             dim=c(n_years + 1000,  n_iters))

  for (i in 2:n_years) x[i,] <- ctr$rho * x[i-1,] + sqrt(1-ctr$rho^2) * x[i,]
  x <- ctr$cv * x
  
  # take a subset of samples, ensures that there is potential a bias
  # in the assessment year (does not matter if looking at long term
  # equilibrium)
  
  k <- 100:(n_years + 1000 - 100)    # ignore the 1st 100
  firstSample <- sample(k,1)
  lastSample  <- firstSample + n_years -1
  x <- x[firstSample:lastSample,]
  for (i in 1:n_hrates) d[,i,] <- x
  
  # CHECK THIS:
  ## ad hoc error setup in the 1st year, fixed for iCod age range
  #d$N[(nR+1):n_ages,1,,] <- (1/ctr$Year1Bias)*N1[(nR+1):n_ages]*rep(exp(d$assError[1,,]),14)
  return(d)
}

#' @title HCR: Setup of weight error structure
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param d XXX
#' @param ctr XXX
#' 
hcr_set_wgtErrors <- function(d,ctr) 
  {
  # Weight error - note for first year
  #                no cv in this implementation, need to be added
  # NOTE - same error is applied to all ages - should include an option
  #        for white noise accross ages.
  n_ages <- dim(d)[1]
  n_years  <- dim(d)[2]
  n_hrates <- dim(d)[3]
  n_iters  <- dim(d)[4]
  
  x <- array(rnorm(n_years * n_iters),
             dim=c(n_years,  n_iters))
  
  for (y in 2:n_years) x[y,] <- ctr$rho * x[y-1,] + sqrt(1 - ctr$rho^2) * x[y,]
  x <- ctr$cv * x
  for (a in 1:n_ages) {
    for (h in 1:n_hrates) d[a,,h,] <- x
  }
  
  return(d)

}

#' @title HCR: Setup of recruitment error structure
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param d XXX
#' @param ctr XXX
#' 
hcr_set_recErrors <- function(d,ctr) 
{
  
  n_years  <- dim(d)[1]
  n_hrates <- dim(d)[2]
  n_iters  <- dim(d)[3]
  
  # Recruitment: cv & autocorrelation
  x <- array(rnorm(n_years * n_iters),
             dim=c(n_years , n_iters))
  
  for (y in 2:n_years) x[y,] <- ctr$rho * x[y-1,] + sqrt(1 - ctr$rho^2) * x[y,]
  x <- exp(x*ctr$cv)
  for (h in 1:n_hrates) d[,h,] <- x
  
  return(d)
}

#' @title HCR: Reading starting years input from file
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param file XXX
#' 
hcr_read_startfile <- function(file) {
  
  d <- list()
  
  indata    <- matrix(scan(file,0,quiet=TRUE),ncol=18,byrow=TRUE)
  d$age     <- indata[,1]     # age classes
  d$N       <- indata[,2]     # population (000s)
  d$N_cv    <- indata[,3]     # population cv
  d$sW      <- indata[,4]     # spawning weight (kg)
  d$sW_cv   <- indata[,5]     # spawning weight cv
  d$cW      <- indata[,6]     # catch weight (kg)
  d$cW_cv   <- indata[,7]     # catch weight cb
  d$mat     <- indata[,8]     # maturity
  d$mat_cv  <- indata[,9]     # maturity cv
  d$selF    <- indata[,10]    # fishing mortality (selection pattern)
  d$selF_cv <- indata[,11]    # fishing mortality cv
  d$pF      <- indata[,12]    # proportion of fishing mort. bf. spawning
  d$selD    <- indata[,13]    # discard mortality
  d$selD_cv <- indata[,14]    # discard mortality cv
  d$M       <- indata[,15]    # natural mortality
  d$M_cv    <- indata[,16]    # natural mortality cv
  d$pM      <- indata[,17]    # proportio of natural mort. bf. spawning
  d$selB    <- indata[,18]    # selection pattern of the "fishable biomass"
  
  return(d)
}

#' @title HCR: Set starting condition for stock
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param dat_y1 XXX
#' @param d XXX
#' @param ctr XXX
#' @param wgt_error_type XXX
#' @param refBweight XXX
#' 

hcr_set_starting_conditions <- function(dat_y1, d, ctr, wgt_error_type, refBweight) 
  {
  

  d$N[,1,,]   <- dat_y1$N
  d$sW[,,,]   <- dat_y1$sW   # spawning weight (kg)
  d$cW[,,,]   <- dat_y1$cW   # catch weight (kg)
  d$mat[,,,]  <- dat_y1$mat  # maturity
  d$selF[,,,] <- dat_y1$selF # fishing mortality (selection pattern)
  d$pF[,,,]   <- dat_y1$pF   # proportion of fishing mort. bf. spawning
  d$selD[,,,] <- dat_y1$selD # discard mortality
  d$M[,,,]    <- dat_y1$M    # natural mortality
  d$pM[,,,]   <- dat_y1$pM   # proportio of natural mort. bf. spawning
  d$selB[,,,] <- dat_y1$selB # selection pattern of the "fishable biomass"
  
  n_ages <- length(dat_y1$age)
  n_noRec <- sum(dat_y1$N == 0)
  ## ad hoc error setup in the 1st year
  d$N[(n_noRec+1):n_ages,1,,] <- (1 / ctr$y1Bias) * d$N[(n_noRec+1):n_ages] * rep(exp(d$assError[1,,]),n_ages-1)
  
  
  d$TAC[1,,]  <- ctr$tac_y1 # Already set TAC in the assessment year (year 1)
  d$TAC[2,,]  <- ctr$tac_y2 # Already set TAC in the advisory year (year 2)
  
  n_years <- dim(d$cW)[2]
  if(wgt_error_type == 1) {
    d$cW[,2:n_years,,] <- d$cW[,2:n_years,,] * exp(d$cvcW[,2:n_years,,])
    d$sW[,2:n_years,,] <- d$sW[,2:n_years,,] * exp(d$cvsW[,2:n_years,,])
  }
  
  if(wgt_error_type == 2) {
    d$cW[,2:n_years,,] <- d$cW[,2:n_years,,] * (1+d$cvcW[,2:n_years,,])
    d$sW[,2:n_years,,] <- d$sW[,2:n_years,,] * (1+d$cvsW[,2:n_years,,])
  }
  
  if(refBweight == 0) d$bW <- d$sW   # use stock weights to calculate ref bio
  if(refBweight == 1) d$bW <- d$cW   # use catch weights to calculate ref bio
  
  return(d)
}




#' @title TAC2Fmult
#' 
#' @description XXX
#' 
#' @export 
#' 
#' @param d XXX
#' @param y XXX
#' @param h XXX
#' 
hcr_TAC_to_Fmult <- function(d,y,h) {

  TAC <- d$TAC[y,h,]
  Na  <-  d$N[,y,h,]
  Sa  <-   d$selF[,y,h,]
  Da  <-  d$selD[,y,h,]
  Ma  <-  d$M[,y,h,]
  Wa  <-  d$cW[,y,h,]
  
  epsilon <- 1e-04
  Ba <- Na * Wa

  B <- colSums(Ba)

  TAC <- ifelse(TAC > 0.9 * B, 0.9 * B, TAC)
  Fmult <- TAC/colSums(Ba * Sa * exp(-Ma)) + 0.05
  for (i in 1:5) {
    Fa <- t(Fmult * t(Sa))
    Za <- Fa + Ma
    Y1 <- colSums(Fa/Za * (1 - exp(-Za)) * Ba)
    Fa <- t((Fmult + epsilon) * t(Sa))
    Za <- Fa + Ma
    Y2 <- colSums(Fa/Za * (1 - exp(-Za)) * Ba)
    dY <- (Y2 - Y1)/epsilon
    Fmult <- Fmult - (Y2 - TAC)/dY
  }
  Fmult <- ifelse(TAC == 0, 0, Fmult)
  Fmult <- ifelse(Fmult < 0, 0, Fmult)
  return(Fmult)
}

#' @title Add implementation error to TAC
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param TACy2 XXX
#' @param TACy1 XXX
#' @param beta1 XXX
#' @param beta2 XXX

hcr_implementation <- function (TACy2, TACy1, beta1, beta2) 
  {
  i <- TACy2 < TACy1
  if (sum(i) > 0) 
    TACy2[i] <- TACy2[i] * (TACy1[i]/TACy2[i])^(rbeta(1,beta1, beta2))
  return(TACy2)
}

#' @title Operating model
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param d XXX
#' @param y XXX
#' @param h XXX
#' @param ctr ctr_rec
#' @param Fmult XXX
#' @param nR XXX

hcr_operating_model <- function(d, y, h, ctr, Fmult, nR=1) {
  
  n_ages  <- dim(d$N)[1]
  n_years <- dim(d$N)[2]
  
  N   <- d$N[,y ,h,]
  cW  <- d$cW[,y,h,]
  sW  <- d$sW[,y,h,]
  xN <- d$mat[,y,h,]
  M   <- d$M[,y,h,]
  pF <- d$pF[,y,h,]
  pM <- d$pM[,y,h,]
  
  tF <- t(Fmult*t(d$selF[,y,h,]))
  dF <- t(Fmult*t(d$selD[,y,h,]))
  d$tF[,y,h,] <- tF
  
  # Conventional ssb
  ssb <- colSums(N * exp( -(pM * M + pF * (tF + dF))) * xN * sW)
  ## Mean age in the spawning stock
  # mAge <- colSums((SSBay*c(1:n_ages)))/ssb 
  ## Egg mass
  # ssb <- colSums(Ny*exp(-(My*pMy+(Fy+Dy)*pFy))*maty*sWy * (0.005*sWy))
  
  
  d$N[1,y,h,] <- hcr_recruitment_model(ssb = ssb, d$cvR[y, h,], ctr=ctr)
  
  N[1,] <- d$N[1,y,h,] # update the recruits in the current year
  # TAKE THE CATCH
  d$C[,y,h,] <- N * tF/ (tF + dF + M + 0.00001)*(1-exp(- (tF + dF + M)))
  # NEXT YEARS STOCK

  NyEnd <- N * exp( -( tF + dF + M))
  if(y < n_years ) {
    d$N[2:n_ages, y+1, h,] <- NyEnd[1:(n_ages-1),]
    # plus group calculation
    d$N[n_ages,y+1,h,] <- d$N[n_ages,y+1,h,] +
      d$N[n_ages,y,h,] * exp(-d$tF[n_ages,y,h,] - d$M[n_ages,y,h,])
  }
  return(d)
}

#' @title HCR recruitment model
#' 
#' @description XXX
#' 
#' @export
#' 

#' @param ssb XXX
#' @param cv XXX
#' @param ctr XXX
#' 
hcr_recruitment_model <- function(ssb,cv,ctr) {
  rec <- switch(ctr$model,
                recruit1(ssb,ctr$ssb_break,ctr$r_mean,cv),   # Hockey Stick
                recruit2(ssb,ctr$ssb_break,ctr$rec_mean,cv)) # Bootstrap deviation
  return(rec)
}


#' @title Hockey stick recruitment model
#' 
#' @description XXX
#'
#' @export
#' 
#' @param ssb XXX
#' @param ssbcut XXX
#' @param recmean XXX
#' @param reccv XXX
recruit1 <- function (ssb, ssbcut, recmean, reccv) 
  {
  rec <- ifelse(ssb >= ssbcut, 1, ssb/ssbcut) * recmean * reccv
  return(rec)
}

#' @title Bootstrap model
#' 
#' @description Just a dummy for now
#'
#' @export
#' 
#' @param ssb XXX
#' @param ssbcut XXX
#' @param recmean XXX
#' @param rdev XXX
recruit2 <- function (ssb, ssbcut, recmean, rdev) {
  rec <- ifelse(ssb >= ssbcut, 1, ssb/ssbcut) * recmean * rdev
  return(rec)
}

# ------------------------------------------------------------------------------
# New versions

# Na       <- d$N[,year + delay,h,]
# Wa       <- d$bW[,year,h,]
# SelB     <- d$selB[,year+delay,h,]
# bio      <- colSums(Na * Wa * SelB)
# hrate    <- HRATE[h] * hcr_ctr$iter
# assError <- d$assError[year,h,]

#' @title Observation error model
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param d XXX
#' @param y XXX
#' @param h XXX
#' @param hrate The harvest rate (fishing mortality)
#' @param Fmult XXX
#' @param delay XXX
#' @param ass_bias A constant
#' @param ass_error_type XXX

hcr_observation_error <- function(d, y, h, hrate, Fmult, delay, ass_bias, ass_error_type) {
  
  assError <- d$assError[ y + delay, h,]
  
  N   <-    d$N[,y + delay, h,]
  bW  <-   d$bW[,y + delay, h,]
  sB  <- d$selB[,y + delay, h,]
  bio <- colSums(N * bW * sB)
  
  sW  <-   d$sW[,y + delay ,h,]
  xN  <-  d$mat[,y + delay, h,]
  M   <-    d$M[,y + delay, h,]
  pF  <-   d$pF[,y + delay, h,]
  pM  <-   d$pM[,y + delay, h,]
  
  totalF <- t(Fmult * t(d$selF[, y + delay, h,]))
  
  dF <- t(Fmult * t(d$selD[, y + delay, h,]))
  
  ssb <- colSums(N * exp( -( pM * M + pF * (totalF + dF))) * xN * sW)
  
  n_iters <- dim(d$N)[4]
  hrate    <- rep(hrate, n_iters)
  
  
  
  
  
  ## A. The assessment error model
  if (ass_error_type == 1) {
    bio_hat     <- bio   * ass_bias * exp(assError)
    ssb_hat     <- ssb   * ass_bias * exp(assError)
    hrate_hat   <- hrate * ass_bias * exp(assError)
  }
  
  if (ass_error_type == 2) {
    bio_hat     <- bio   * ass_bias * (1 + assError)
    ssb_hat     <- ssb   * ass_bias * (1 + assError)
    hrate_hat   <- hrate * ass_bias * (1 + assError)
  }
  
  return(list(hrate=hrate_hat,bio=bio_hat,ssb=ssb_hat))
}


#' @title Effort type management
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param hrate_hat XXX
#' @param ssb_hat XXX
#' @param Btrigger XXX


hcr_management_effort <- function(hrate_hat,ssb_hat,Btrigger) 
  {
  i <- ssb_hat < Btrigger  
  hrate_hat[i] <- hrate_hat[i] * ssb_hat[i]/Btrigger
  
  return(hrate_hat)
}

#' @title F-based Harvest Control Rule
#' 
#' @description The F-based rule is the conventional ICES decision rule. Here it
#' is implemented such that the TAC next year is calculated from the true
#' stock in numbers based on a fishing mortality that includes observation error.
#' 
#' If the Btrigger is set in the rule (Btrigger > 0) then linear reductions of 
#' fishing mortality is done relative to observed spawning stock biomass (i.e.
#' that includes observation errrors).
#' 
#' @export
#' 
#' @param d XXX
#' @param y XXX
#' @param h XXX
#' @param delay integer If 0 next years TAC (y+1) is based on stock in numbers
#' in the assessment year (y). If 1 next years TAC(y+1) is based on stock in
#' numbers in the advisory year (y+1).
#' @param hrate_hat vector Target fishing mortality with observation noise
#' included.
#' @param ssb_hat vector Observed spawning stock biomass.
#' @param Btrigger numeric Btrigger of the HCR. The harvest rate is linearily
#' reduced for spawning biomass below Btrigger. Setting Btrigger to 0 is
#' equivalent to a HCR where target fishing mortality is constant, irrespective
#' of spawning stock status.
#' @note Need to check is ssb-hat is calculated according to the correct delay
#' specification. 
#' 
#' To do: Modify function so that buffer is not active below Btrigger and
#' also include a TAC-constraint, either the
#' Icelandic type or the convention percentage contraint used in EU stocks.
 
hcr_management_fmort <- function(d,y,h,delay,hrate_hat,ssb_hat,Btrigger)
  {
  
  selF     <- d$selF[,y + delay,h,]
  selD     <- d$selD[,y + delay,h,]
  Na       <- d$N[,y + delay,h,]
  cWa      <- d$cW[,y + delay,h,]
  selF     <- d$selF[,y + delay,h,]
  selD     <- d$selD[,y + delay,h,]
  Ma       <- d$M[,y + delay,h,] 
  
  # adjust harvest rate
  i <- ssb_hat < Btrigger  
  hrate_hat[i] <- hrate_hat[i] * ssb_hat[i]/Btrigger  
  
  Fa <- t(hrate_hat * t(selF))
  Da <- t(hrate_hat * t(selD))

  tac_y2 <- colSums(Na * Fa/(Fa + Da + Ma + 1e-05) * 
                             (1 - exp(-(Fa + Da + Ma))) * cWa)
  
  return(tac_y2)
}

#' @title Biomass-based Harvest Control Rule
#' 
#' @description The Biomass-based HCR is used in the case of the Icelandic cod
#' and saithe. Here it is implemented such that the TAC next year is a multiplier
#' of the target harvest rate and the observed reference biomass (i.e. that 
#' includes observation errrors).
#' 
#' If the Btrigger is set in the rule (Btrigger > 0) then linear reductions of 
#' fishing mortality is done relative to observed spawning stock biomass (i.e.
#' that includes observation errrors).
#' 
#' @export
#' 
#' @param bio_hat vector Observed reference biomass.
#' @param ssb_hat vector Observed spawning stock biomass.
#' @param hrate vector Target harvest rate.
#' @param Btrigger numeric Btrigger of the HCR. The harvest rate is linearily
#' reduced for spawning biomass below Btrigger. Setting Btrigger to 0 is
#' equivalent to a HCR where target fishing mortality is constant, irrespective
#' of spawning stock status.
#' @param tac_y1 numeric Current year's TAC
#' @param tac_alpha Weight of current TAC in the HCR. 0 = none, 1 = constant TAC model
#' 
#' 
#' @note To do: Modify function so that buffer is not active below Btrigger and
#' also to a EU type TAC-constraint.
#' 
hcr_management_bio <- function(bio_hat,ssb_hat,hrate,Btrigger,tac_y1,tac_alpha)
  {
  i <- ssb_hat < Btrigger  
  hrate[i] <- hrate[i] * ssb_hat[i]/Btrigger
  
  tac_y2 <- hrate * bio_hat
  tac_y2 <- tac_alpha * tac_y1 + (1 - tac_alpha) * tac_y2
  
  return(tac_y2)
}