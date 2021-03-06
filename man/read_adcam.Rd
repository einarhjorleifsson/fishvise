% Generated by roxygen2 (4.0.1): do not edit by hand
\name{read_adcam}
\alias{read_adcam}
\title{Reads 'adcam' assessment results}
\usage{
read_adcam(path, run, rName = NA, mName = NA, calcSurBio = T,
  ggFactor = T, Scale = 1000, assYear = NA, retroY = NA)
}
\arguments{
\item{path}{Path to the runs, here the 'root' path to the runs.}

\item{run}{Name of the \emph{directory} that contains the result.}

\item{rName}{Name of the run.}

\item{mName}{Name of the model used.}

\item{calcSurBio}{Flag, TRUE (default) if survey biomass should be calculated.}

\item{ggFactor}{If TRUE (default) rescale prerecruits with M=0.0}

\item{Scale}{Convertion of values}

\item{assYear}{Assessment year}

\item{retroY}{The retrospective year}
}
\value{
A list with \code{data.frame} rby, rbya and rba
}
\description{
Some longer text here
}
\seealso{
\code{\link{read_separ}} for reading separate model output and \code{\link{read_adapt}} for reading adapt model output
}

