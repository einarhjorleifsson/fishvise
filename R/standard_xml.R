#' @title rvk2icesxml
#' 
#' @description Converts data.frame to ices xml format for standard graphs.
#' Note that currently takes only data by year. Need to implement for the yield
#' per recruit data.
#' 
#' @export
#' 
#' @param rby A data frame containing stock summary data
#' @param FishStock XXX
#' @param AssessmentYear XXX
#' @param RecruitmentAge XXX
#' @param FAge XXX
#' @param rba data.frame containing yield per recruit input. If missing it is
#' ignored.
#' @param Flim XXX
#' @param Fpa XXX
#' @param Blim XXX
#' @param Bpa XXX
#' @param FMSY XXX
#' @param MSYBtrigger XXX
#' @param Fmanagement XXX
#' @param Bmanagement XXX

#' @param RecruitmentLength XXX
#' @param UnitsWeigths XXX
#' @param UnitsRecruits XXX
#' @param TypeLandings XXX
rvk2icesxml <- function(rby,FishStock,AssessmentYear,RecruitmentAge,FAge,rba,UnitsWeigths="tonnes",
                        UnitsRecruits="millions",TypeLandings="official",
                        Flim=0,Fpa=0,Blim=0,Bpa=0,
                        FMSY=0,MSYBtrigger=0,Fmanagement=0,Bmanagement=0,RecruitmentLength=0) 
  {
  
  # Header data:
  h1  <- "<?xml version='1.0' encoding='utf-8' standalone='no'?>"
  h2  <- "<?xml-stylesheet type='text/xsl' href='StandrdGraphsStyle.xsl'?>"
  h3  <- "<Assessment xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='ICES_Standard_Graphs.xsd'>"

  h4  <- paste0("<FishStock>",FishStock,"</FishStock>")
  h5  <- paste0("<AssessmentYear>",AssessmentYear,"</AssessmentYear>")
  h6  <- paste0("<Flim>",Flim,"</Flim>")
  h7  <- paste0("<Fpa>",Fpa,"</Fpa>")
  h8  <- paste0("<Blim>",Blim,"</Blim>")
  h9  <- paste0("<Bpa>",Bpa,"</Bpa>")
  h10 <- paste0("<FMSY>",FMSY,"</FMSY>")
  h11 <- paste0("<MSYBtrigger>",MSYBtrigger,"</MSYBtrigger>")
  h12 <- paste0("<Fmanagement>",Fmanagement,"</Fmanagement>")
  h13 <- paste0("<Bmanagement>",Bmanagement,"</Bmanagement>")
  h14 <- paste0("<RecruitmentAge>",RecruitmentAge,"</RecruitmentAge>")
  h14b <- paste0("<FAge>",FAge,"</FAge>")
  h15 <- paste0("<RecruitmentLength>",RecruitmentLength,"</RecruitmentLength>")
  h16 <- paste0("<UnitsWeigths>",UnitsWeigths,"</UnitsWeigths>")
  h17 <- paste0("<UnitsRecruits>",UnitsRecruits,"</UnitsRecruits>")
  h18 <- paste0("<TypeLandings>",TypeLandings,"</TypeLandings>")
  
  header <- paste0(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h14b,h15,h16,h17,h18)
  ## Stock summary data
  # the wrappers
  w0 <- c("<FishData>","</FishData>")
  w1 <- c("<Year>","</Year>")
  w2 <- c("<Recruitment>","</Recruitment>")
  w3 <- c("<TBiomass>","</TBiomass>")
  w4 <- c("<SSB>","</SSB>")
  w5 <- c("<Landings>","</Landings>")
  w6 <- c("<YieldSSB>","</YieldSSB>")
  w7 <- c("<F_Landings>","</F_Landings>")
  
  # the loop

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
  
  res <- paste0(header,tmp,"</Assessment>")
  return(res)
}
