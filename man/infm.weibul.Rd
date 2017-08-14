\name{infm.weibul}

\alias{infm.weibul}

\title{Observed information matrix for fixed regression parameter of interest}

\description{Matrix as a component of modifying part of regression parameters: observed information matrix for fixed number of parameter of interest}

\usage{infm.weibul(Y, X, sigma, phi, delta, whc)}

\arguments{
  \item{Y}{log of Weibull distributed failure times}
  \item{X}{covariate matrix}
  \item{sigma}{given value of scale parameter of extreme value distribution}
  \item{phi}{given values of regression parameters of extreme value distribution}
  \item{delta}{Censoring status, coded as 0 (censored observation) and 1 (uncersored observation) binary integer variable}
  \item{whc}{Set position of regression parameter of interest corresponding predefined covariate matrix. It will take integer value from 1 to number of regression parameters}
}

\value{Symmetric matrix of dimension n x n (n is number of regression parameter).}

\references{
Barndorff-Nielsen (1980). Conditionality resolutions. Biometrika, 67(2): 293-310.

Barndorff-Nielsen (1983). On a formula for the distribution of the maximum likelihood estimator. Biometrika, 70(2): 343-365.

Khan M. H. R. and Shaw J. E. H (2016). Variable selection for survival data with a class of adaptive elastic net techniques. Statistics and Computing, 26(3): 725-741.

Islam, M. M., Khan, M. H. R. and Hawlader T. (2015). Modified profile likelihood estimation for the weibull regression models in survival analysis. Submitted.
}

\author{Mazharul Islam and Hasinur Rahaman Khan}

\seealso{LX.mat.weibull}

\examples{

dat <- data.weibull(n = 20, shape=2, regco=c(2,1.5,3,2.5))

par=c(1,1,1,1,1,1)

infm.weibul(Y=log(dat$ftime),X=model.matrix(ftime~x1+x2+x3+x4,data=dat),
sigma=2,phi=matrix(par[-1],ncol=1),delta=dat$delta,whc=2)

par=c(1,1,1)
infm.weibul(Y=log(dat$ftime),X=model.matrix(ftime~x1,data=dat),sigma=2,
phi=matrix(par[-1],ncol=1),delta=dat$delta,whc=2)

}
