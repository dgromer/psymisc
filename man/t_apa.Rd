% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apa.R
\name{t_apa}
\alias{t_apa}
\title{Report t-Test in APA style}
\usage{
t_apa(x, es = "cohens_d", format = c("text", "markdown", "rmarkdown",
  "html", "latex", "docx", "plotmath"), info = FALSE, print = TRUE)
}
\arguments{
\item{x}{a call to \code{t_test}}

\item{es}{character specifying the effect size to report. One of
\code{"cohens_d"} (default), \code{"hedges_g"} or \code{"glass_delta"} if
\code{x} is an independent samples t-test. Ignored if \code{x} is a paired
samples or one sample t-test (cohen's d is reported for these test).}

\item{format}{character string specifying the output format. One of
\code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
\code{"latex"}, \code{"docx"} or \code{"plotmath"}.}

\item{info}{logical indicating whether to print a message on the used test
(default is \code{FALSE})}

\item{print}{logical indicating wheter to print the formatted output via
\code{cat} (\code{TRUE}, default) or return as character string.}
}
\description{
Report t-Test in APA style
}
\examples{
t_apa(t_test(sens_seek ~ group, hquest))

}

