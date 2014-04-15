\docType{package}
\name{omnivr}
\alias{omnivr}
\alias{omnivr-package}
\title{Simple univariate and bivariate tests and plots, data type independent.}
\description{
Provides two main functions which are both omnivores: the
busy user can feed them data without needing to worry too
much about data types: - nominal, ordinal, integer or
continuous. That is why the package is called
\code{omnivr}: omnivorous Visualisation and Reporting.
}
\details{
The two main functions are: \code{otest} for conducting
simple tests of statistical significance \code{oplot}, a
minimalist wrapper for \code{ggplot2}. \code{ggplot2} is
great for providing highly customisable plots. \code{oplot}
makes it just a little bit quicker to provide the simplest
plots - bar charts, scatterplots etc - with a consistent
appearance and interface across data types.

The main use case for \code{omnivr} is providing quick
overview reports from the results of a questionnaire
survey: providing tests of main variables against
background, often sociodemographic, variables which are
usually of mixed data type - nominal, ordinal, continuous
etc. \code{oplot} provides a function otest which conducts
the statistical tests and provides a p-value. By default,
if \code{oplot} is provided with two variables, and the
corresponding \code{oplot} test for those two variables is
not significant, the plot is not produced, though the
returned empty string does contain informative attributes.
}
\seealso{
Other main: \code{\link{classer}}; \code{\link{oplot}};
\code{\link{otest}}
}

