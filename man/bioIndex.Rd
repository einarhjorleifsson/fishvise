\name{bioIndex}
\alias{bioIndex}
\title{Calculate length based indices}
\usage{
  bioIndex(lengths, stations, sex, lenClass, yr, lwcoeff)
}
\arguments{
  \item{lengths}{\emph{data.frame} containing length
  measurement for a given species in a given survey year.}

  \item{stations}{\emph{data.frame} containg station survey
  station informations.}

  \item{sex}{\emph{boolean} A flag indicating if indices
  should also be compiled by sex}

  \item{lenClass}{\emph{numerical vector} containing length
  classes to compile the indices for}

  \item{yr}{\emph{numerical} value for year}

  \item{lwcoeff}{\emph{numerical vector} of length 2,
  containing parameter a and b of the length weight
  relationship.}
}
\description{
  Calculates abundance and biomass survey indices based on
  length classes for a particular species in a given year.
}

