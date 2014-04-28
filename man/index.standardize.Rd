\name{index.standardize}
\alias{index.standardize}
\title{Standardizes value by tow length}
\usage{
index.standardize(value, towlength, std.length = 4, std.width = 17,
  to.area = FALSE, min.length, max.length)
}
\arguments{
  \item{value}{The value to standardize (e.g. number of
  fish)}

  \item{towlength}{the reported towlength}

  \item{std.length}{The tow length to standardize, default
  4 nautical miles}

  \item{std.width}{The width swept, default 17 m}

  \item{to.area}{A flag indicating if results should be by
  area, default FALSE}

  \item{min.length}{Minimum tow length, if missing set to
  half the standardized tow length}

  \item{max.length}{Maxium tow length, if missing set to
  twice the standardized tow length}
}
\description{
Standaridize value of interest. This is equivalent to the
first part of the function \code{\link{Calc.index}}.
}

