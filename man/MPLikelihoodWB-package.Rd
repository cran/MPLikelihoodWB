\name{MPLikelihoodWB-package}
\docType{package}
\title{Modified Profile Likelihood Estimation for Weibull Shape and Regression Parameters}
\description{Makes adjustment of profile likelihood function of parameter of interest in presence of many nuisance parameters under Weibull regression models. Modified profile likelihood techniue is developed by extending the Barndorff-Nielsens approach for Weibull regression models in presence of collinearity among covariates. Modified likelihood estimates are provided.}
\details{A modification to profile likelihood is introduced to overcome from this type of problems. Several adjustments have been proposed to modify the profile likelihood function. In an earlier paper named conditionality resolutions by Barndorff-Nielsen published in Biometrica (1980, 1983), the construction of ancillary statistics and the expressions for the conditional distribution of the maximum likelihood estimate, was discussed for transformation models and exponential models, with most emphasis on the latter.

The main goal is to obtain adjustment to the profile likelihood function when the parameters of interest are, firstly, shape parameter beta (or, alternatively, kappa) and secondly, regression parameter phi in presence of collinearity among the covariates in Weibul lregression model. Note that practically correlated covariates are found in many areas of biostatistics including microarray, genetics, medical statistics. The presence of
collinearity makes sometimes the estimation and inference procedure problematic and complex and the model as a whole can be found as statistically significant but the individual regression coefficients can not be (e.g. Khan and Shaw, 2015). The modified profile likelihood estimators outperform the profile likelihood estimators in terms of three statistical measures as comparison criterion- mean squared errors, bias and standard errors.
\tabular{ll}{
Package: \tab MPLikelihoodWB\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-1-1\cr
License: \tab GPL-2\cr
Depends: \tab survival, MASS\cr}
}

\author{Mazharul Islam and Hasinur Rahaman Khan
Maintainer: Hasinur Rahaman Khan <hasinurkhan@gmail.com>
}

\references{
Barndorff-Nielsen (1980). Conditionality resolutions. Biometrika, 67(2), 293-310.

Barndorff-Nielsen (1983). On a formula for the distribution of the maximum likelihood
estimator. Biometrika, 70(2), 343-365.

Khan M. H. R. and Shaw J. E. H (2015). Variable selection for survival data with a class of adaptive elastic net techniques. Statistics and Computing,  DOI 10.1007/s11222-015-9555-8.

Islam, M. M. and Khan, M. H. R. (2015). Modified profile likelihood estimation for the weibull regression 
models in survival analysis. Submitted.}

\keyword{Modified profile likelihood}
\keyword{Profile likelihood}
\keyword{Weibull regression model}

\examples{
# For modified profile likelihood estimation
dat1 <- data.weibull(n=35, nco=5)
mpl.wb<-Mprofile.wb(formula=ftime~x1+x2+x3+x4+x5, censor= "delta", data=dat1)
pl.wb<-survreg(Surv(ftime, delta)~x1+x2+x3+x4+x5, data=dat1, dist="weibull")

# For random dataset creation with given shape
dat2 <- data.weibull.shape(35, nco=5, shape=3)
mpl.wb<-Mprofile.wb(formula=ftime~x1+x2+x3+x4+x5, censor="delta", data=dat2)
pl.wb<-survreg(Surv(ftime, delta)~x1+x2+x3+x4+x5, data=dat2, dist="weibull")

# For correlated data with a given shape
dat3 <- data.weibull.reg(35, ncop=5, shape=2)
}
