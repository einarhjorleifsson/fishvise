\name{index.stratify}
\alias{index.stratify}
\title{Calculates stratified indices}
\usage{
index.stratify(sfile, col.names = c("strata", "v"), std.length = 4,
  std.width = 17, use.rallarea = TRUE, strata.list, std.cv = 1)
}
\arguments{
  \item{sfile}{data.frame with strata and abundance indices
  of each tow}

  \item{col.names}{character vector containing strata name
  and value of interest}

  \item{std.length}{tow length}

  \item{std.width}{tow width, std.length * std.width gives
  area swept}

  \item{use.rallarea}{Use rall area}

  \item{strata.list}{strata list}

  \item{std.cv}{Standard cv}
}
\description{
Calculate indices by strata. If standardization by tow
length is desired use first
\code{\link{index.standardize}}. This captures the second
part of the function \code{\link{Calc.index}}.
}

