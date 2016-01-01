\name{mplik.wb.bi}
\alias{mplik.wb.bi}
\title{Modified Profile Likelihood Function of Weibull Regression Parameters}
\description{
Modified profile likelihood function of Weibull regression parameters. The function considers one regression parameter at a time as parameter of interest and remaining parameters as nuisance parameters. Standard optimization procedures are required to find the estimate of certain regression parameter at a time.
}
\usage{
mplik.wb.bi(par, Y, X, delta, whc)
}
\arguments{
  \item{par}{Initial value of parameters to be estimated by optimization}
  \item{Y}{Weibull distributed failure times}
  \item{X}{covariate matrix}
  \item{delta}{Censoring status, coded as 0 for censored observation and 1 for uncersored observation}
  \item{whc}{Set position of regression parameter of interest corresponding predefined covariate matrix. It will take integer value from 1 to number of regression parameters}
}
\value{Negative log likelihood of the function at given value of parameters and data. Optimization of this function will produce maximum likelihood estimate of regression parameter of Extreme value distribution. Transformation will be required to obtain estimate of Weibull regression parameter.}
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

\seealso{mplik.wb.s}
\examples{
dat <- data.weibull(n=35,nco=3) # create dummy data with 3 covariates
mwbi<-mplik.wb.bi(par=c(1,1,1,1,1),Y=dat$ftime,X=model.matrix(ftime~x1+x2+x3,data=dat),
delta=dat$delta,whc=2)
mwbi
}
\keyword{Weibull regression model}
