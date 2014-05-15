\name{xc}
\alias{xc}
\title{Utility function which when given a string including at least one instance of the
separator element returns a vector of strings.
When you want \code{x= c("red","blue","green") }
you can just type \code{x=xc("red blue green") }}
\usage{
xc(stri, sepp = " ")
}
\arguments{
  \item{stri}{A string, in quotes}

  \item{sepp}{A string, usually a single character, to
  separate the elements of \code{stri}.}
}
\value{
A vector of strings.
}
\description{
Utility function which when given a string including at
least one instance of the separator element returns a
vector of strings. When you want \code{x=
c("red","blue","green") } you can just type \code{x=xc("red
blue green") }
}
\examples{
xc("red green blue")
xc("Croatia;Serbia;Bosnia and Herzegovina",sepp=";")
}
\seealso{
Other utility functions: \code{\link{wplot}}
}

