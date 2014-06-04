\name{findr}
\alias{findr}
\title{Returns a block of variables based on their attributes}
\usage{
findr(att = "block", dat = data.r, equ = "x", datafilename = "", ...)
}
\arguments{
  \item{dat}{dataset}

  \item{att}{the attribute}

  \item{equ}{the value of the attribute. If equ is "list",
  then findr will return a list.}

  \item{datafilename}{useful if you are working with
  different data files. wprop applies this. Used by otrans
  and oprop when submitting a list of variables or blocks}
}
\value{
A dataset
}
\description{
Note the dataset it returns also has a label attribute
which tries to extract an appropriate label from the
attribute
}
\examples{
Here are some examples
}
\seealso{
Other experimental: \code{\link{dummy.code.lab}};
\code{\link{ggheat}}; \code{\link{loopr2}};
\code{\link{loopr}}; \code{\link{omnicor}};
\code{\link{otrans_inner}}; \code{\link{outerF}};
\code{\link{pseudocor}}; \code{\link{sigHeat}};
\code{\link{writeprop}}; \code{\link{xt}}
}

