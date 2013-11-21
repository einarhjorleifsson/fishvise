\name{run.srmsymc}
\alias{run.srmsymc}
\title{Run srmsymc}
\usage{
  run.srmsymc(sr, nits = 100, path, output = FALSE)
}
\arguments{
  \item{sr}{integer. containg the stock-recruitment model}

  \item{nits}{Number of iterations of bootstrapping - if 0,
  does only the deterministic fit}

  \item{path}{characters. Name of the directory for storing
  the output. If missing, the output remains in the working
  directory.}

  \item{output}{boolean. If FALSE (default) no results
  (*.dat) files are returned.}
}
\description{
  Run the ADM srmsymc program
}
\author{
  Einar Hjorleifsson <einar.hjorleifsson
}

