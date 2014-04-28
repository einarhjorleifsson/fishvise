\name{read.rbx}
\alias{read.rbx}
\title{Reads 'adcam' assessment results}
\usage{
read.rbx(path, run, rName = NA, mName = NA, calcSurBio = T,
  ggFactor = T, Scale = 1000, assYear = NA, retroY = NA,
  std.names = TRUE)
}
\arguments{
  \item{path}{Path to the runs, here the 'root' path to the
  runs.}

  \item{rName}{Name of the run.}

  \item{mName}{Name of the model used.}

  \item{calcSurBio}{Flag, TRUE (default) if survey biomass
  should be calculated.}

  \item{ggFactor}{If TRUE (default) rescale prerecruits
  with M=0.0}

  \item{Scale}{Convertion of values}

  \item{assYear}{Assessment year}

  \item{retroY}{The retrospective year}

  \item{std.names}{Boolean, if TRUE (default) column names
  are standardize, if FALSE the original names are retained
  and returned.}

  \item{rec.column}{Character, name of recruitment column
  (e.g. "n2", "n3").}
}
\value{
A list with \code{data.frame} rby, rbya and rba
}
\description{
Some longer text here
}
\seealso{
\code{\link{read.separ}} for reading separate model output
and \code{\link{read.adapt}} for reading adapt model output
}

