\name{read.separ}
\alias{read.separ}
\title{Reads 'separable' assessment results}
\usage{
  read.separ(path, run, rName = NA, mName = NA,
    calcSurBio = F, ggFactor = T, Scale = 1000,
    assYear = NA, retroY = NA)
}
\arguments{
  \item{path}{Path to the runs, here the 'root' path to the
  runs.}

  \item{run}{Name of the \emph{directory} that contains the
  result.}

  \item{rName}{Name of the run.}

  \item{mName}{Name of the model used.}

  \item{calcSurBio}{Flag, TRUE (default) if survey biomass
  should be calculated.}

  \item{ggFactor}{If TRUE (default) rescale prerecruits
  with M=0.0}

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
\note{
  If file exist it is simply overwritten without any
  warning
}
\seealso{
  \code{\link{read.adcam}} for reading adcam model output
  and \code{\link{read.adapt}} for reading adapt model
  output
}

