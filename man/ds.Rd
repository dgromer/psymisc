% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.R
\name{ds}
\alias{ds}
\title{Descriptive statistics}
\usage{
ds(.data, formula, funs = c("mean", "se"), names = funs, ...)
}
\arguments{
\item{.data}{a data frame containing the variables in the formula
\code{formula}.}

\item{formula}{a formula in the form \code{lhs ~ rhs} where \code{lhs} is a
numeric variable giving the data values and \code{rhs} one or more factors
with two or more levels giving the corresponding groups or \code{.} if the
parameters should be calculated over all cases.}

\item{funs}{character vector with function names indicating the parameters to
calculate (default: \code{c("mean", "se")}).}

\item{names}{character vector with variable names for output data frame,
defaults to \code{funs}.}

\item{...}{further arguments passed to \code{funs}, e.g. \code{na.rm}}
}
\description{
A wrapper for dplyr's \code{group_by} and \code{summarise} for descriptive
statistics.
}
\examples{
ds(sleep, extra ~ group)
ds(sleep, extra ~ group, funs = c("median", "var", "n"))

ds(hquest, trait_anx ~ group + gender)
}
\seealso{
\link{aggregate}
}

