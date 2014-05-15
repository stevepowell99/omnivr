\name{xmb}
\alias{xmb}
\title{More robust test of whether something is NA or null.
Should not give an error or NA, whatever x is.}
\usage{
xmb(x, y = "")
}
\arguments{
  \item{x}{The object to be tested. May not even exist.}

  \item{y}{true if missing or null or y, otherwise false.
  NOTE IT GIVES F IF IT IS ANY DATA FRAME, EVEN AN EMPTY
  ONE}
}
\description{
More robust test of whether something is NA or null. Should
not give an error or NA, whatever x is.
}

