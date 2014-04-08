\docType{package}
\name{quickplotr}
\alias{quickplotr}
\alias{quickplotr-package}
\title{Quick ggplots}
\description{
ggplot2 is great for providing highly customisable plots.
quickplotr makes it just a little bit quicker to provide
the simplest plots - bar charts, scatterplots etc - with a
consistent appearance.
}
\details{
The main use case for quickplotr is providing quick
overview reports from the results of a questionnaire
survey: providing tests of main variables against
background, often sociodemographic, variables which are
usually of mixed data type - nominal, ordinal etc.
quickplotr provides a function quicktestr which conducts
the statistical tests and provides a p-value. By default,
if quickplotr is provided with two variables, and the
corresponding quickplotr test for those two variables is
not significant, the plot is not produced, though the
returned empty string does contain informative attributes.
}
\seealso{
Other main: \code{\link{qplotr}}; \code{\link{quicktestr}}
}

