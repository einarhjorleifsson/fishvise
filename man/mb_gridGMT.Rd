\name{mb_gridGMT}
\alias{mb_gridGMT}
\title{Gridding data using nearneighbor gmt function}
\usage{
mb_gridGMT(data, square, out.file, I = 0.064, S = 0.128, debug = F)
}
\arguments{
  \item{data}{The xyz-data}

  \item{square}{The square to limit the gridding to}

  \item{out.file}{Name of outputted grid file}

  \item{I}{The XXX. Default value 0.064}

  \item{S}{The XXX. Default value 0.128}

  \item{debug}{A boolean flag. if TRUE returns the
  generated command without running the code. If TRUE, runs
  gmt-nearneighbor.}
}
\description{
XXX
}

