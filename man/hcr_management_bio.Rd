\name{hcr_management_bio}
\alias{hcr_management_bio}
\title{Biomass-based Harvest Control Rule}
\usage{
  hcr_management_bio(bio_hat, ssb_hat, hrate, Btrigger,
    tac_y1, tac_alpha)
}
\arguments{
  \item{bio_hat}{vector Observed reference biomass.}

  \item{ssb_hat}{vector Observed spawning stock biomass.}

  \item{hrate}{vector Target harvest rate.}

  \item{Btrigger}{numeric Btrigger of the HCR. The harvest
  rate is linearily reduced for spawning biomass below
  Btrigger. Setting Btrigger to 0 is equivalent to a HCR
  where target fishing mortality is constant, irrespective
  of spawning stock status.}

  \item{tac_y1}{numeric Current year's TAC}

  \item{tac_alpha}{Weight of current TAC in the HCR. 0 =
  none, 1 = constant TAC model}
}
\description{
  The Biomass-based HCR is used in the case of the
  Icelandic cod and saithe. Here it is implemented such
  that the TAC next year is a multiplier of the target
  harvest rate and the observed reference biomass (i.e.
  that includes observation errrors).

  If the Btrigger is set in the rule (Btrigger > 0) then
  linear reductions of fishing mortality is done relative
  to observed spawning stock biomass (i.e. that includes
  observation errrors).
}
\note{
  To do: Modify function so that buffer is not active below
  Btrigger and also to a EU type TAC-constraint.
}

