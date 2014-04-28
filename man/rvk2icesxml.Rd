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
  \item{rby}{A data frame containing stock summary data}

  \item{FishStock}{XXX}

  \item{AssessmentYear}{XXX}

  \item{RecruitmentAge}{XXX}

  \item{FAge}{XXX}

  \item{rba}{data.frame containing yield per recruit input.
  If missing it is ignored.}

  \item{Flim}{XXX}

  \item{Fpa}{XXX}

  \item{Blim}{XXX}

  \item{Bpa}{XXX}

  \item{FMSY}{XXX}

  \item{MSYBtrigger}{XXX}

  \item{Fmanagement}{XXX}

  \item{Bmanagement}{XXX}

  \item{RecruitmentLength}{XXX}

  \item{UnitsWeigths}{XXX}

  \item{UnitsRecruits}{XXX}

  \item{TypeLandings}{XXX}
}
\description{
Converts data.frame to ices xml format for standard graphs.
Note that currently takes only data by year. Need to
implement for the yield per recruit data.
}

