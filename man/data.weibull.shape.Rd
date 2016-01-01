\name{data.weibull.shape}
\alias{data.weibull.shape}
\title{Grnerate Random Dataset for A Given Shape Parameter}
\description{Generate random data set of weibull distributed failure time, covariates and corresponding censoring status. Covariates are drawn from uniform(0, 1) distribution. All regression parameters have set to 1. All of these have been arbitrarily choosen.}
\usage{data.weibull.shape(n, nco, shape)}
\arguments{
  \item{n}{sample size}
  \item{nco}{number of covariates}
  \item{shape}{value of shape parameter}}
\value{Data frame}

\references{
Islam, M. M. and Khan, M. H. R. (2015). Modified profile likelihood estimation for the weibull regression 
models in survival analysis. Submitted.
}
\author{Mazharul Islam and Hasinur Rahaman Khan}

\seealso{data.weibull.reg}
\examples{
dat<-data.weibull.shape(35,nco=5,shape=3)
dat
}
