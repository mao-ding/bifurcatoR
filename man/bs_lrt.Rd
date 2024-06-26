% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bs_lrt.R
\name{bs_lrt}
\alias{bs_lrt}
\title{bootstrap likelihood ratio tests for mixR fits}
\usage{
bs_lrt(x, H0 = 1, H1 = 2, family = "normal", nboot = 100, iter = 1000, ...)
}
\arguments{
\item{x}{raw data for a mixR fit (vector or 3-column matrix)}

\item{H0}{the number of components in the null model (1)}

\item{H1}{the number of components in the alternative model (2)}

\item{family}{fit distribution ("normal","weibull","gamma","lognormal")}

\item{nboot}{number of bootstraps to perform (100)}

\item{iter}{maximum iterations for EM algorithm (1000)}

\item{...}{additional arguments, passed to mixR::bs.test()}
}
\value{
an object of class "bootEM" with three items (see mixR::bs.test)
}
\description{
The mixR package provides fast implementations for a number of 1-dimensional
mixture models, along with the bs.test function for performing bootstrap
likelihood ratio tests of component numbers (default is "one lump or two?").
Note that the fitting and bootstrapping of non-normal mixtures can be
substantially sped up by binning the data. See mixR::bin for details.
}
\seealso{
mixR::bin

mixR::mixfit

mixR::bs.test

mixR
}
