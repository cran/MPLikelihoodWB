\name{Mprofile.wb}
\alias{Mprofile.wb}
\title{Modified Profile Likelihood Estimation of Weibull Shape and Regression Parameter}
\description{Modified profile likelihood estimation of Weibull shape and regression parameter. The methodology was addopted from Conditionality resolutions which is actually the construction of ancillary statistics and expressions for the conditional distribution of the maximum likelihood estimate of a statistical model. The result will produce less bias with minimum mean square error at least for Weibull shape parameter. Performances of profile and modified profile likelihood estimation are differentiable when sample size is reasonably small.}
\usage{Mprofile.wb(formula, censor, data, initial)}
\arguments{
  \item{formula}{an object of class formula}
  \item{censor}{Censoring status, coded as 0 for censored observation and 1 for uncersored observation}
  \item{data}{Data frame of weibull distributed failure time, covariates and censoring variable}
  \item{initial}{Initial values of the parameters at which likelihood function will be optimized}
}
\details{Initial values of the parameters at which likelihood function will be optimized. Default value is 1 for all parameters. To change initial values input a vector of numeric values with length of number of parameters to be optimized. First initial value is attributed for shape parameter. For example, use vector c(2,3,2,3,4) as initial value for shape and four egression parameters.}

\value{
\item{Formula}{an object of class formula}
\item{Coefficients}{estimates of the regresion parameters}
\item{Scale}{estimate of scale parameter of Weibull model}}
\references{
Barndorff-Nielsen (1983). On a formula for the distribution of the maximum likelihood estimator. Biometrika, 70(2), 343-365.

Islam, M. M. and Khan, M. H. R. (2015). Modified profile likelihood estimation for the weibull regression models in survival analysis. Submitted.}
\author{Mazharul Islam and Hasinur Rahaman Khan}
\seealso{summary.Mprofile.wb}
\examples{dat3 <- data.weibull.reg(35, ncop=5, shape=2)
mpw<-Mprofile.wb(formula = ftime~x1+x2+x3+x4+x5, censor="delta", data=dat3)
mpw
}
\keyword{Modified profile likelihood}
\keyword{Profile likelihood}
\keyword{Weibull regression model}
