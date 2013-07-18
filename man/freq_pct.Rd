\name{freq_pct}
\alias{freq_pct}
\title{Cross tablulation of frequencies and percentages}
\usage{
  freq_pct(varlist, crossvar, varlabs, useNA = "no")
}
\arguments{
  \item{varlist}{a \code{\link{list}} of variables/vectors
  to tabulate.}

  \item{crossvar}{a variable/vector to tablulate
  \code{varlist} across.}

  \item{varlabs}{a character vector of length
  \code{length(varlist)} that contains variable labels.}

  \item{useNA}{passed to \code{\link{table}}}
}
\description{
  \code{freq_pct} returns a data frame containting cross
  tabulated frequencies and column percentages for a given
  list of variables.
}
\details{
  ...
}

