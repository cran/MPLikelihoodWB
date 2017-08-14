\name{mplik.wb.s}
\alias{mplik.wb.s}
\title{
Modified profile likelihood function of Weibull shape parameter
}
\description{
Modified profile likelihood function of Weibull shape parameter. The function considers shape parameter as parameter of interest and remaining parameters as nuisance parameters. Standard optimization procedures are required to find the estimate of shape parameter. The estimate will be less biased comparing to existing methods when sample size is considerably small.       
}
\usage{
mplik.wb.s(par, Y, X, delta)
}
\arguments{
  \item{par}{
  Initial value of parameters to be estimated by optimization 
}
  \item{Y}{
Weibull distributed failure times
}
  \item{X}{
covariate matrix
}
  \item{delta}{
Censoring status, coded as 0(censored observation) and 1(uncersored observation) binary integer variable. 
}
}

\value{
Negative log likelihood of the function at given value of parameters and data. Optimization of this function will produce maximum likelihood estimate of scale parameter of Extreme value distribution. Transformation will be required to obtain estimate of Weibull shape parameter.  
}
\references{
Barndorff-Nielsen (1980). Conditionality resolutions. Biometrika, 67(2) : 293-310.

Barndorff-Nielsen (1983). On a formula for the distribution of the maximum likelihood
estimator. Biometrika, 70(2) : 343-365.

Khan M. H. R. and Shaw J. E. H (2016). Variable selection for survival data with a class of adaptive elastic net techniques. Statistics and Computing, 26(3): 725-741.

Islam, M. M., Khan, M. H. R. and Hawlader T. (2015). Modified profile likelihood estimation for the weibull regression 
models in survival analysis. Submitted.
}
\author{
Mazharul Islam and Hasinur Rahaman Khan
}

\seealso{
mplik.wb.bi
}
\examples{
dat <- data.weibull(n = 40, shape=2, regco=c(2,1.5,3,2.5))

mplik.wb.s(par=c(1,1,1,1,1,1),Y=dat$ftime,X=model.matrix(ftime~x1+x2+x3+x4,data=dat),
delta=dat$delta)
}

\keyword{Weibull regression model}
