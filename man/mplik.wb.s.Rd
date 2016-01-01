\name{mplik.wb.s}
\alias{mplik.wb.s}
\title{Modified Profile Likelihood Function of Weibull Shape Parameter}
\description{
Modified profile likelihood function of Weibull shape parameter. The function considers shape parameter as parameter of interest and remaining parameters as nuisance parameters. Standard optimization procedures are required to find the estimate of shape parameter. The estimate will be less biased comparing to existing methods when sample size is considerably small.       
}
\usage{mplik.wb.s(par, Y, X, delta)}
\arguments{
  \item{par}{Initial value of parameters to be estimated by optimization}
  \item{Y}{Weibull distributed failure times}
  \item{X}{covariate matrix}
  \item{delta}{Censoring status, coded as 0 for censored observation and 1 for uncersored observation}
}
\value{
Negative log likelihood of the function at given value of parameters and data. Optimization of this function will produce maximum likelihood estimate of scale parameter of Extreme value distribution. Transformation will be required to obtain estimate of Weibull shape parameter.  
}
\references{
Barndorff-Nielsen (1980). Conditionality resolutions. Biometrika, 67(2), 293-310.

Barndorff-Nielsen (1983). On a formula for the distribution of the maximum likelihood
estimator. Biometrika, 70(2), 343-365.

Islam, M. M. and Khan, M. H. R. (2015). Modified profile likelihood estimation for the weibull regression 
models in survival analysis. Submitted.
}
\author{
Mazharul Islam and Hasinur Rahaman Khan
}

\examples{
dat <- data.weibull(n=35,nco=3) # create dummy data with 3 covariates
mws<-mplik.wb.s(par=c(1,1,1,1,1),Y=dat$ftime,X=model.matrix(ftime~x1+x2+x3,data=dat),
delta=dat$delta)
mws
}
\keyword{Weibull regression model}
