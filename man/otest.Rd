\name{otest}
\alias{otest}
\title{Provide a simple bivariate test for two variables depending on the types of the variables.
Also provides some basic statistics for a single variable}
\usage{
otest(xx, yy = NULL, spv = FALSE, ...)
}
\arguments{
  \item{xx}{A vector.}

  \item{yy}{If provided, a second vector of the same length
  as xx.}

  \item{level1}{Force type of first variable}

  \item{level2}{Force type of second variable}

  \item{spv}{Whether to simulate p values. At the moment
  only used for chi-squared tests. May take a while.}
}
\value{
The p-value, with additional information provided as
attributes.
}
\description{
Provide a simple bivariate test for two variables depending
on the types of the variables. Also provides some basic
statistics for a single variable
}
\examples{
con=mtcars$wt
ord=factor(mtcars$gear,ordered=T)
nom=factor(mtcars$cyl,ordered=F)

otest(nom,ord)
#same as:
otest_nom_ord(nom,ord)

#users can provide their own functions to overwrite functions provided by the package.
otest_nom_ord=function(xx,yy,spv)
{      
  pp=kruskal.test(factor(xx,ordered=T),yy)
  p=pp$p.value
  attr(p,"estimate")=pp$statistic
  attr(p,"method")="The user has changed just this text as an exercise"
  p
}
otest_nom_ord(nom,ord)


#users can define their own data types and provide corresponding functions.
attr(ord,"setlevout")="myNewDataType"
otest_nom_myNewDataType=function(xx,yy,spv)
{      
  pp=kruskal.test(factor(xx,ordered=T),yy)
  p=pp$p.value
  attr(p,"estimate")=pp$statistic
  attr(p,"method")="The user has changed just this text once again as an exercise"
  p
}
otest_nom_myNewDataType(nom,ord)
}
\seealso{
Other main omnivr functions: \code{\link{classer}};
\code{\link{omnivr}}, \code{\link{omnivr-package}};
\code{\link{oplot}}; \code{\link{sankplot}}
}

