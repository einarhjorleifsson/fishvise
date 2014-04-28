\name{rvk2icesxml}
\alias{rvk2icesxml}
\title{rvk2icesxml}
\usage{
rvk2icesxml(rby, FishStock, AssessmentYear, RecruitmentAge, FAge, rba,
  UnitsWeigths = "tonnes", UnitsRecruits = "millions",
  TypeLandings = "official", Flim = 0, Fpa = 0, Blim = 0, Bpa = 0,
  FMSY = 0, MSYBtrigger = 0, Fmanagement = 0, Bmanagement = 0,
  RecruitmentLength = 0)
}
\arguments{
  \item{rby}{A data frame containing stock summary data.
  Required column names are: \itemize{ \item \emph{year} -
  year \item \emph{r} - recruitment \item \emph{bio} -
  total or reference biomass \item \emph{ssb} - spawning
  stock biomass \item \emph{y} - yield, \item \emph{f} -
  fishing mortality. }}

  \item{FishStock}{A character vector containing stock
  "name". e.g. "cod-farp", "cod-iceg".}

  \item{AssessmentYear}{An integer representing assessment
  year}

  \item{RecruitmentAge}{An integer representing recruitment
  age}

  \item{FAge}{A character vector containg reference fishing
  moralites, e.g. "F5-10"}

  \item{rba}{\\code{data.frame} containing yield per
  recruit input. If missing it is ignored. If supplied
  required column names are: \itemize{ \item \emph{age} -
  age \item \emph{M} - natural mortality \item \emph{Mat} -
  maturity \item \emph{PF} - partial fishing mortality
  before spawning \item \emph{PM} - partial natural
  mortality before spawning \item \emph{WeSt} - spawning
  stock weights \item \emph{f} - fishing mortality \item
  \emph{WeCa} - catch weights } This part has not been
  tested yet.}

  \item{Flim}{A numeric value}

  \item{Fpa}{A numeric value}

  \item{Blim}{A numeric value}

  \item{Bpa}{A numeric value}

  \item{FMSY}{A numeric value}

  \item{MSYBtrigger}{A numeric value}

  \item{Fmanagement}{A numeric value}

  \item{Bmanagement}{A numeric value}

  \item{RecruitmentLength}{A numeric value}

  \item{UnitsWeigths}{A character, e.g. "tonnes"}

  \item{UnitsRecruits}{A character, e.g. "millions"}

  \item{TypeLandings}{A character, e.g. "official", "ices
  estimates".}
}
\description{
Converts data.frame to ices xml format for standard graphs.
Note that currently takes only data by year. Need to
implement for the yield per recruit data.
}

