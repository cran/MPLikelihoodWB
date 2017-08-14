\name{data.weibull}
\alias{data.weibull}
\title{
Random data set generating function. 
}
\description{
Generate random data set of weibull distributed failure time, covariates and corresponding censoring status with a given shape and a set of regression parameters. Correlated covariates can also be drawn with a given number of correlated covariates.
}
\usage{
data.weibull(n, shape = 2, regco = c(1, 3), rcen = 0.25, ncorvar = 3, 
correlated = FALSE)
}

\arguments{
  \item{n}{
sample size
}
  \item{shape}{
value of shape parameter
}
  \item{regco}{
vector of regression parameters that corresponds to covariates, for correlated = FALSE
}
  \item{rcen}{
censoring rate
}
  \item{ncorvar}{
no of correlated covariates, for correlated = TRUE. See details below.
}
  \item{correlated}{
logical; if true correlated covariates will be generated with a given no of correlated covariates
}
}
\details{
ncorvar is non required if correlated = FALSE  and regco is not required if correlated = TRUE. 
}
\value{
Data frame with columns:
\item{ftime}{lifetime data from weibull distribution}
\item{x}{covariates}
\item{delta}{censoring status, 0 or 1. A value 0 indicates corresponding observation is censored}
}

\author{
Mazharul Islam and Hasinur Rahaman Khan
}

\examples{
data.weibull(n = 20)
data.weibull(n = 20, shape=1.7, regco=c(2,1,3,4))
data.weibull(n = 20, shape=1.5, ncorvar=4, correlated=TRUE)
}

\keyword{Simulated Weibull regression data}
\keyword{Weibull data with correlated covariates}
