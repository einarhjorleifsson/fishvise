\name{read.rbya}
\alias{read.rbya}
\title{Reads results-by-year-and-age}
\usage{
read.rbya(file, format = "rvk", cn = c("xx"), info = FALSE,
  name_stock = NA, yAss = NA, aR = NA, aP = NA, aF = c(NA, NA),
  pM = rep(0.25, 14), pF = rep(0.1, 14), Run = NA, Model = NA)
}
\arguments{
  \item{file}{If it does not contain an absolute path, the
  file name is relative to the current working directory,
  getwd().}

  \item{format}{Character specifying the origin of the
  input file. So far implemented "rvk" and "abd" format.}

  \item{cn}{Names of columns to return for "rvk" format.}

  \item{info}{boolean If FALSE (default) only data.frame
  with XXX returned. If TRUE a list containing the
  data.frame and a list containing the additional
  information is returned. See return for additional XXX.}

  \item{name_stock}{character. Stock name.}

  \item{yAss}{integer. Assessment year}

  \item{aR}{integer. Age of recruitment.}

  \item{aP}{integer. Plus group age}

  \item{aF}{vector. Elements contain youngest and oldest
  age used to calculate the reference fishing mortality.}

  \item{pM}{XXX}

  \item{pF}{XXX}

  \item{Run}{Run name (spring survey, fall survey, both
  surveys, etc ..)}

  \item{Model}{Model name (Adcam, Adapt, Separable, etc
  ..)}
}
\description{
XXX
}
\examples{
file <- paste(path.package("fishvise"),'extdata/resultsbyyearandage',sep='/')
read.rbya(file)
}

