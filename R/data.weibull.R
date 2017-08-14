data.weibull <-
function(n, shape=2, regco=c(1,3), rcen=0.25, ncorvar=3, correlated=FALSE){
   if(shape<=0)
   stop("Shape must be a positive quantity")
   if(any(regco<0))
   stop("regco must not have any negative value. \n Data generation has been limited to positive valued regression coefficients only.")
   ncovar <- length(regco)  
   delta <- rbinom(n,1,(1-rcen))
   intercept <- matrix(1,ncol=1,nrow=n)
 if(correlated == FALSE){
   data.w <- matrix(runif(n*ncovar),ncol=ncovar,nrow=n)
   colnames(data.w) <- paste("x",1:ncovar,sep="") 
   X <- cbind(intercept,data.w)
   Xbeta <- X%*%matrix(c(1,regco),ncol=1,nrow=ncol(X))
   ftime <- rweibull(n,shape=shape,scale=Xbeta)
   wbdat <- data.frame(ftime,data.w,delta)
   return(wbdat)
 }else{
   if(ncorvar<2)
   stop("ncorvar must >= 2 for correlated covariates")
   mu <- rep(5,ncorvar)
   Sigma <- matrix(0.7,nrow=ncorvar,ncol=ncorvar)+diag(ncorvar)*0.25
   data.wr <- mvrnorm(n, mu,Sigma)
   colnames(data.wr) <- paste("x",1:ncorvar,sep="")
   X <- cbind(intercept,data.wr)
   Xbeta <- as.matrix(X)%*%matrix(1,ncol=1,nrow=ncol(X))
   ftime <- rweibull(n,shape=shape,scale=Xbeta)
   wbdat <- data.frame(ftime,data.wr,delta)
   return(wbdat)
}
}
