\name{hcr_management_fmort}
\alias{hcr_management_fmort}
\title{F-based Harvest Control Rule}
\usage{
  hcr_management_fmort(d, y, h, delay, hrate_hat, ssb_hat,
    Btrigger)
}
\arguments{
  \item{d}{XXX}

  \item{y}{XXX}

  \item{h}{XXX}

  \item{delay}{integer If 0 next years TAC (y+1) is based
  on stock in numbers in the assessment year (y). If 1 next
  years TAC(y+1) is based on stock in numbers in the
  advisory year (y+1).}

  \item{hrate_hat}{vector Target fishing mortality with
  observation noise included.}

  \item{ssb_hat}{vector Observed spawning stock biomass.}

  \item{Btrigger}{numeric Btrigger of the HCR. The harvest
  rate is linearily reduced for spawning biomass below
  Btrigger. Setting Btrigger to 0 is equivalent to a HCR
  where target fishing mortality is constant, irrespective
  of spawning stock status.}
}
\description{
  The F-based rule is the conventional ICES decision rule.
  Here it is implemented such that the TAC next year is
  calculated from the true stock in numbers based on a
  fishing mortality that includes observation error.

  If the Btrigger is set in the rule (Btrigger > 0) then
  linear reductions of fishing mortality is done relative
  to observed spawning stock biomass (i.e. that includes
  observation errrors).
}
\note{
  Need to check is ssb-hat is calculated according to the
  correct delay specification.

  To do: Modify function so that buffer is not active below
  Btrigger and also include a TAC-constraint, either the
  Icelandic type or the convention percentage contraint
  used in EU stocks.
}

