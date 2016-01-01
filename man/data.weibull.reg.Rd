\name{data.weibull.reg}
\alias{data.weibull.reg}
\title{Grnerate Random Dataset with Correlated Covariates}
\description{Generate random data set of weibull distributed failure time, covariates and corresponding censoring status. Covariates are drawn from multivariate normal distribution as if they are correlated. All regression parameters have set to 1. All of these have been arbitrarily choosen.}

\usage{data.weibull.reg(n, ncop, shape)}

\arguments{
  \item{n}{sample size}
  \item{ncop}{number of covariates}
  \item{shape}{value of shape parameter}
}

\value{Data frame}

\references{
Khan M. H. R. and Shaw J. E. H (2015). Variable selection for survival data with a class of adaptive elastic net techniques. Statistics and Computing,  DOI 10.1007/s11222-015-9555-8.

Islam, M. M. and Khan, M. H. R. (2015). Modified profile likelihood estimation for the weibull regression 
models in survival analysis. Submitted.
}
\author{Mazharul Islam and Hasinur Rahaman Khan}
\seealso{data.weibull}
\examples{
#Generate the data from Weiull model
dat<-data.weibull.reg(n=35,ncop=5,shape=2)
dat
}
