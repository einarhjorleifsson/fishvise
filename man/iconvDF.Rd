\name{iconvDF}
\alias{iconvDF}
\title{Converts character columns in a \code{data.frame} between Encodings

Does the same as \code{iconv} but on a data.frame instead of a vector}
\usage{
  iconvDF(data, from = "ISO8859-1", to = "UTF-8", ...)
}
\arguments{
  \item{data}{data.frame to convert}

  \item{from}{A character string describing the current
  encoding, default is "ISO8859-1"}

  \item{to}{A character string describing the target
  encoding}

  \item{...}{Other things to pass to \code{iconv}}
}
\value{
  data.frame
}
\description{
  Converts character columns in a \code{data.frame} between
  Encodings

  Does the same as \code{iconv} but on a data.frame instead
  of a vector
}
\author{
  Einar
}
\seealso{
  \code{\link{iconv}}
}

