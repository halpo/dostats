\name{compose}
\alias{\%.\%}
\alias{compose}
\alias{composition}
\alias{nest}
\title{Nest functions}
\usage{
  compose(..., .list)

  x \%.\% y
}
\arguments{
  \item{...}{functions to be nested together}

  \item{.list}{alternatively an explicit list of functions.
  If specified \code{...} will be ignored.}

  \item{x}{a function}

  \item{y}{a function}
}
\value{
  new function consisting of the functions nested
}
\description{
  Nest functions
}
\details{
  compose creates a functional composition of the listed
  functions. Functional composition of functions f and g is
  defined as f(g(.)). Order matters the right most function
  listed will be the innermost function in the composition,
  same with the operator version. To remember the order
  lists will be the order read out, ie. compose(f,g) =
  f(g(x))

  When using the operator version it is good to remember
  that parentheses are recommended see the examples
}
\examples{
compose(any, is.na)(c(NA,1:3))
(sum\%.\%is.na)(c(1,NA))  #correct
\dontrun{
sum\%.\%is.an(NA)  #incorrect
}
}
\author{
  Andrew Redd
}
\keyword{misc}
\keyword{utilities,}

