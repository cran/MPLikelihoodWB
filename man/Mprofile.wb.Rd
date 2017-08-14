\name{Mprofile.wb}
\alias{Mprofile.wb}
\title{
Modified profile likelihood estimation of Weibull shape and regression parameter}
\description{
Modified profile likelihood estimation of Weibull shape and regression parameter. The methodology was addopted from 'Conditionality resolutions' which is actually "the construction of ancillary statistics and expressions for the conditional distribution of the maximum likelihood estimate of a statistical model". The result will produce less bias with minimum mean square error; at least for Weibull shape parameter. Performances of profile and modified profile likelihood estimation are differentiable when sample size is reasonably small.   
}
\usage{
Mprofile.wb(formula, censor, data, method = "BFGS", initial = 1)
}
\arguments{
  \item{formula}{
an object of class "formula".
}
  \item{censor}{
censoring status, coded as 0(censored observation) and 1(uncersored observation) binary integer variable in the data frame.
}
  \item{data}{
data frame of weibull distributed failure time, covariates and censoring variable.
}
  \item{method}{
method under which optimization is performed. Other methods are "Nelder-Mead",  "CG", "L-BFGS-B",  "SANN", and "Brent". 
}
  \item{initial}{
Initial values of the parameters at which likelihood function will be optimized. Default value is 1 for all parameters. To change initial values input a vector of numeric values with length of number of parameters to be optimized. First initial value is attributed for shape parameter. For example, use vector c(2,3,2,3,4) as initial value for shape and four regression parameters. 
}
}

\value{
The function is a list with atleast the following component:
\item{Formula}{an object of class "formula"}
\item{Coefficients}{estimates of the regresion parameters}
\item{Scale}{estimate of scale parameter}
}
\references{
Barndorff-Nielsen (1980). Conditionality resolutions. Biometrika, 67(2) : 293-310.

Barndorff-Nielsen (1983). On a formula for the distribution of the maximum likelihood
estimator. Biometrika, 70(2) : 343-365.

Khan M. H. R. and Shaw J. E. H (2015). Variable selection for survival data with a class of adaptive elastic net techniques. Statistics and Computing,  DOI: 10.1007/s11222-015-9555-8.

Islam, M. M., Khan, M. H. R. and Hawlader T. (2015). Modified profile likelihood estimation for the weibull regression 
models in survival analysis. Submitted.
}
\author{
Mazharul Islam and Hasinur Rahaman Khan
}

\seealso{
optim
}
\examples{
dat <- data.weibull(n = 40, shape=2, regco=c(2,1.5,3,2.5))

Mprofile.wb(formula=ftime~x1+x2+x3+x4,censor="delta",data=dat)

survreg(Surv(ftime,delta)~x1+x2+x3+x4,data=dat,dist="weibull")
}

\keyword{Modified profile likelihood}
\keyword{Profile likelihood}
\keyword{Weibull regression model}
