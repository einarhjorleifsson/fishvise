#' @title rvk2icesxml
#' 
#' @description Converts data.frame to ices xml format for standard graphs.
#' Note that currently takes only data by year. Need to implement for the yield
#' per recruit data.
#' 
#' @export
#' 
#' @param rby A data frame containing stock summary data. Required column names
#' are:
#' \itemize{
#'  \item \emph{year} - year
#'  \item \emph{r} - recruitment
#'  \item \emph{bio} - total or reference biomass
#'  \item \emph{ssb} - spawning stock biomass
#'  \item \emph{y} - yield,
#'  \item \emph{f} - fishing mortality.
#' }
#' @param FishStock A character vector containing stock "name". e.g. "cod-farp", "cod-iceg".
#' @param AssessmentYear An integer representing assessment year
#' @param RecruitmentAge An integer representing recruitment age
#' @param FAge A character vector containg reference fishing moralites, e.g. "F5-10"
#' @param rba \\code{data.frame} containing yield per recruit input. If missing it is
#' ignored. If supplied required column names are:
#' \itemize{
#'  \item \emph{age} - age
#'  \item \emph{M} - natural mortality
#'  \item \emph{Mat} - maturity
#'  \item \emph{PF} - partial fishing mortality before spawning
#'  \item \emph{PM} - partial natural mortality before spawning
#'  \item \emph{WeSt} - spawning stock weights
#'  \item \emph{f} - fishing mortality
#'  \item \emph{WeCa} - catch weights
#' }
#' This part has not been tested yet.
#' @param Flim A numeric value
#' @param Fpa A numeric value
#' @param Blim A numeric value
#' @param Bpa A numeric value
#' @param FMSY A numeric value
#' @param MSYBtrigger A numeric value
#' @param Fmanagement A numeric value
#' @param Bmanagement A numeric value
#' @param RecruitmentLength A numeric value
#' @param UnitsWeigths A character, e.g. "tonnes"
#' @param UnitsRecruits A character, e.g. "millions"
#' @param TypeLandings A character, e.g. "official", "ices estimates".
rvk2icesxml <- function(rby,FishStock,AssessmentYear,RecruitmentAge,FAge,rba,UnitsWeigths="tonnes",
                        UnitsRecruits="millions",TypeLandings="official",
                        Flim=0,Fpa=0,Blim=0,Bpa=0,
                        FMSY=0,MSYBtrigger=0,Fmanagement=0,Bmanagement=0,RecruitmentLength=0) 
  {
  
  # Header data:
  h1  <- "<?xml version='1.0' encoding='utf-8' standalone='no'?>\n"
  h2  <- "<?xml-stylesheet type='text/xsl' href='StandrdGraphsStyle.xsl'?>\n"
  h3  <- "<Assessment xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='ICES_Standard_Graphs.xsd'>\n"

  h4  <- paste0("<FishStock>",FishStock,"</FishStock>\n")
  h5  <- paste0("<AssessmentYear>",AssessmentYear,"</AssessmentYear>\n")
  h6  <- paste0("<Flim>",Flim,"</Flim>\n")
  h7  <- paste0("<Fpa>",Fpa,"</Fpa>\n")
  h8  <- paste0("<Blim>",Blim,"</Blim>\n")
  h9  <- paste0("<Bpa>",Bpa,"</Bpa>\n")
  h10 <- paste0("<FMSY>",FMSY,"</FMSY>\n")
  h11 <- paste0("<MSYBtrigger>",MSYBtrigger,"</MSYBtrigger>\n")
  h12 <- paste0("<Fmanagement>",Fmanagement,"</Fmanagement>\n")
  h13 <- paste0("<Bmanagement>",Bmanagement,"</Bmanagement>\n")
  h14 <- paste0("<RecruitmentAge>",RecruitmentAge,"</RecruitmentAge>\n")
  h14b <- paste0("<FAge>",FAge,"</FAge>\n")
  h15 <- paste0("<RecruitmentLength>",RecruitmentLength,"</RecruitmentLength>\n")
  h16 <- paste0("<UnitsWeigths>",UnitsWeigths,"</UnitsWeigths>\n")
  h17 <- paste0("<UnitsRecruits>",UnitsRecruits,"</UnitsRecruits>\n")
  h18 <- paste0("<TypeLandings>",TypeLandings,"</TypeLandings>\n")
  
  header <- paste0(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h14b,h15,h16,h17,h18)
  
  ## Stock summary data
  # the wrappers
  w0 <- c("<FishData>","</FishData>\n")
  w1 <- c("<Year>","</Year>\n")
  w2 <- c("<Recruitment>","</Recruitment>\n")
  w3 <- c("<TBiomass>","</TBiomass>\n")
  w4 <- c("<SSB>","</SSB>\n")
  w5 <- c("<Landings>","</Landings>\n")
  w6 <- c("<YieldSSB>","</YieldSSB>\n")
  w7 <- c("<F_Landings>","</F_Landings>\n")
  
  # the year loop

  for (i in 1:nrow(rby)) {
    tmp2 <- paste0(w0[1],
                  w1[1],rby$year[i],w1[2],
                  w2[1],rby$r[i],w2[2],
                  w3[1],rby$bio[i],w3[2],
                  w4[1],rby$ssb[i],w4[2],
                  w5[1],rby$y[i],w5[2],
                  w6[1],rby$y[i]/rby$ssb[i],w6[2],
                  w7[1],rby$f[i],w7[2],
                  w0[2])
    if (i == 1) {
      tmp <- tmp2
      } else {
        tmp <- paste0(tmp,tmp2)
      }
  }
  
  ## Yield per recruit input
  if(missing(rba)) {
    return(paste0(header,tmp,"</Assessment>\n"))
  } else {
    # the wrappers
    s0 <- c("<Sensitivity_Data>","</Sensitivity_Data>\n")
    s1 <- c("<Age>","</Age>\n")
    s2 <- c("<M>","</M>\n")
    s3 <- c("<Mat>","</Mat>\n")
    s4 <- c("<PF>","</PF>\n")
    s5 <- c("<PM>","</PM>\n")
    s6 <- c("<WeSt>","</WeSt>\n")
    s7 <- c("<F>","</F>\n")
    s8 <- c("<WeCa>","</WeCa>\n")
    
    # the age loop
    for (i in 1:nrow(rba)) {
      tmp2 <- paste0(s0[1],
                     s1[1],rba$age[i],s1[2],
                     s2[1],rba$M[i],s2[2],
                     s3[1],rba$Mat[i],s3[2],
                     s4[1],rba$PF[i],s4[2],
                     s5[1],rba$PM[i],s5[2],
                     s6[1],rba$WeSt[i],s6[2],
                     s7[1],rba$f,s7[2],
                     s8[1],WeCa[i],s8[2],
                     s0[2])
      if (i == 1) {
        tmp3 <- tmp2
      } else {
        tmp3 <- paste0(tmp3,tmp2)
      }
    }
    return(paste0(header,tmp,tmp3,"</Assessment>\n"))
  }
}
