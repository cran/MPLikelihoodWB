\name{data.weibull}
\alias{data.weibull}
\title{Random Data Set Generating Function}
\description{Generate random data set of weibull distributed failure time, covariates and corresponding censoring status. Covariates are drawn from uniform(0, 1) distribution and the shape parameter is fixed at value 2. All regression parameters are set to 1. The parameters are chosen arbitrarily.}
\usage{data.weibull(n, nco)}
\arguments{
  \item{n}{Sample size}
  \item{nco}{number of covariates}}
\details{The data are generated from weibull distribution.}
\value{Data frame of weibull distributed failure time, covariates and censoring variable}

\references{
Islam, M. M. and Khan, M. H. R. (2015). Modified profile likelihood estimation for the weibull regression 
models in survival analysis. Submitted.
}

\author{Mazharul Islam and Hasinur Rahaman Khan}

\seealso{data.weibull.reg}

\examples{
# For data generated from Weibull distriution
dat.w<-data.weibull(n=35,nco=5)
dat.w
}
\keyword{Weibull}
\keyword{Weibull regression model}
