\name{index.aggregate}
\alias{index.aggregate}
\title{Aggregates stratified indices by users desire}
\usage{
index.aggregate(indices, combine.output)
}
\arguments{
  \item{indices}{Dataframe containing ...}

  \item{combine.output}{A list with ..., if missing uses
  Std.aggregation}
}
\description{
Aggregates stratified indices that have been calculated by
\code{\link{index.stratify}} This captures the second part
of the function \code{\link{Calc.index}}.
}

