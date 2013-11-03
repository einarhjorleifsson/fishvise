\name{Calc.index}
\alias{Calc.index}
\title{Calculate survey indices}
\usage{
  Calc.index(sfile, colname, strata.list, std.toglengd = 4,
    trollbreidd = 17, combine.output = Std.aggregation,
    use.rallarea = T, leidretta.fyrir.toglengd = T, z,
    std.cv = 1, mintoglengd, maxtoglengd)
}
\arguments{
  \item{sfile}{\emph{data.frame} Station table that
  contains columns \code{synis.id} and \code{newstrata} for
  the \code{z}-file. Note that the dataframe does not have
  to include abundance ....}

  \item{colname}{Column to be used as the basis for the
  calculation. These and be \code{fj}, ....}

  \item{strata.list}{\emph{list} that contains the stratas
  to be used as the basis for the calculation. Norally use
  \code{strata.list=ralllist}.}

  \item{std.toglengd}{\emph{numeric} The length of the
  standard tow, default is 4 (miles).}

  \item{trollbreidd}{The swept width, default is 17. The
  area swept of each tow is then calculated as
  \code{std.area <- (std.toglengd * trollbreidd)/1852}
  resulting in standardized area swept of the tow in square
  miles.}

  \item{combine.output}{\emph{list} that contains list of
  stratas upon which the calculation should be aggregated.
  Norally (default) use
  \code{combine.output=Std.aggregation}.}

  \item{use.rallarea}{\emph{boolean} If TRUE (default) then
  strata areas specified in
  \code{attributes(STRATAS)$rall.area} is used. If FALSE
  strata areas specified in \code{attributes(STRATAS)$area}
  is used.}

  \item{leidretta.fyrir.toglengd}{\emph{Boolean} Default
  TRUE \itemize{ \item if \code{toglengd} in \code{sfile}
  \emph{NA} then set \code{toglengd} to \code{std.toglengd}
  \item if \code{toglengd} in \code{sfile} is less than
  \code{mintoglengd} then set \code{toglengd} to
  \code{mintoglengd} \item if \code{toglengd} in
  \code{sfile} is greater than \code{maxtoglengd} then set
  \code{toglengd} to \code{maxtoglengd} }}

  \item{z}{DO NOT UNDERSTAND THIS}

  \item{std.cv}{\emph{numerical} The default coefficient of
  variation. This value will be used in cases when cv can
  not be calculated.}

  \item{mintoglengd}{\emph{numerical} Value repaces
  \code{toglengd} in \code{sfile} if the latter is smaller
  than stated (and if
  \code{leidretta.fyrir.toglengd=TRUE}.}

  \item{maxtoglengd}{Value repaces \code{toglengd} in
  \code{sfile} if the latter is greater than stated (and if
  \code{leidretta.fyrir.toglengd=TRUE}.}
}
\value{
  Returns a \emph{list} with the following
  \emph{data.frame}s: \itemize{ \item \code{$result} with
  the following columns: \itemize{ \item \code{strata}:
  Names/number of the strata \item \code{mean}: Mean
  (number or biomass) within the strata \item \code{stdev}:
  Standard deviation within the strata \item \code{count}:
  Number of tows within the strata \item \code{area}: The
  area (sqmiles) of the strata \item \code{se}: Standard
  error \item \code{cv}: Coefficient of variation \item
  \code{total}: Total (number or biomass) in the strata. }
  \item \code{$Res.names} Contains information about the
  strata names \item \code{$Aggr.output} with the following
  columns: \itemize{ \item \code{mean}: Mean (number or
  biomass) within an area \item \code{se}: Standard error
  within an area \item \code{count}: Number of stations
  within an area ?????? \item \code{area}: The area
  (sqmiles) of the area \item \code{total}: Total (number
  or biomass) in the area. } }
}
\description{
  Original function from Hoski. This function is used in
  \code{\link{bioIndex}}. The function does in principle
  three things: \itemize{ \item Standardizes value (e.g.
  number of fish) by tow length. This is can be done as a
  separate step by using \code{\link{index.standardize}}.
  \item Calculates stratified indices. This can be done as
  a separate step by using \code{\link{index.stratify}}.
  \item Aggregates the stratified indices in various ways.
  This can be done as a separate step by using
  \code{\link{index.aggregate}}. }
}

