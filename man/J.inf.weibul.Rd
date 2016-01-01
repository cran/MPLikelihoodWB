\name{J.inf.weibul}
\alias{J.inf.weibul}
\title{Observed Information Matrix for Fixed K}
\description{ Matrix as a component of modifying part of regression parameters. Observed information matrix for fixed number of parameter of interest k}
\usage{J.inf.weibul(Y, X, sigma, phi, delta, whc)}
\arguments{
  \item{Y}{log of Weibull distributed failure times}
  \item{X}{covariate matrix}
  \item{sigma}{given value of scale parameter of extreme value distribution}
  \item{phi}{given values of regression parameters of extreme value distribution}
  \item{delta}{Censoring status, coded as 0 for censored observation and 1 for uncersored observation}
  \item{whc}{Set position of regression parameter of interest corresponding predefined covariate matrix. It will take integer value from 1 to number of regression parameters}}
\value{Symmetric matrix of dimension n x n (n is number of regression parameter)}

\references{
Barndorff-Nielsen (1980). Conditionality resolutions. Biometrika, 67(2), 293-310.

Barndorff-Nielsen (1983). On a formula for the distribution of the maximum likelihood
estimator. Biometrika, 70(2), 343-365.

Islam, M. M. and Khan, M. H. R. (2015). Modified profile likelihood estimation for the weibull regression 
models in survival analysis. Submitted.
}
\author{Mazharul Islam and Hasinur Rahaman Khan}

\seealso{LX.mat.weibull}

\examples{
dat <- data.weibull(n=35,nco=3) # create dummy data with 3 covariates

par<-c(1,1,1,1,1)

jiw<-J.inf.weibul(Y=log(dat$ftime),X=model.matrix(ftime~x1+x2+x3,data=dat),
sigma=2,phi=matrix(par[-1],ncol=1),delta=dat$delta,whc=2)

jiw
}
