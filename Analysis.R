library(mediation)
library(parallel)
data(framing)

setwd("U:/CMA_sense/Case Study/")
source("multicma.R")

#fit regressions
Y.fit <- lm(immigr ~treat + emo +p_harm + treat*emo + treat*p_harm+ age + educ + gender + income, data=framing)
M.fit <- lm(emo ~ treat + p_harm+ age + educ + gender + income, data=framing)
W.fit <- lm(p_harm~ treat+ age + educ + gender + income, data=framing)

#:::::::::::::::::::::::::
#::::: Table 1 :::::::::::
#:::::::::::::::::::::::::
# point estimates
 point <- multicma(x, Y.fit,M.fit, W.fit, treat="treat", mediator1="p_harm", mediator2="emo")
 names(point) <- c("delta1", "deltaw1", "deltam1", "zeta0", "tau", "deltar1", "zetar0")

# bootstrap
 out.full <- mclapply(1:1000, cma.sense.b,Y.fit=Y.fit, M.fit=M.fit, W.fit=W.fit, treat="treat",
		mediator1="p_harm",mediator2="emo",mc.cores = 1)
                                 
 out.full <- simplify2array(out.full)
 save(out.full, file="result.Rdata")
   
	out <- list()
	out$delta1 <- out.full[1,]
	out$deltaw1 <- out.full[2,]
	out$deltam1 <- out.full[3,]
	out$zeta0 <- out.full[4,]
	out$tau <- out.full[1,]+out.full[4,]
	out$deltar1 <- out.full[6,]
	out$zetar0 <- out.full[7,]
 
	ACME1.95 <- quantile(out$delta1, prob=c(.025, .975), na.rm=TRUE)   
 	ACMEw1.95 <- quantile(out$deltaw1, prob=c(.025, .975), na.rm=TRUE) 
 	ACMEm1.95 <- quantile(out$deltam1, prob=c(.025, .975), na.rm=TRUE) 
 	ANDE0.95 <- quantile(out$zeta0, prob=c(.025, .975), na.rm=TRUE)  
 	ATE.95 <- quantile(out$tau, prob=c(.025, .975), na.rm=TRUE)  
 	ACMEr1.95 <- quantile(out$deltar1 , prob=c(.025, .975), na.rm=TRUE) 
 	ANDEr0.95 <- quantile(out$zetar0 , prob=c(.025, .975), na.rm=TRUE)  


#:::::::::::::::::::::::::
#::::::: Figure 2 ::::::::
#:::::::::::::::::::::::::

#::::::::: Sensitivity analysis
 par(mfrow=c(3,2))
 eta.by =0.05
 gamma  <- seq( 0+eta.by, 0.8-eta.by, 0.01)
 delta  <- seq( 0+eta.by, 0.8-eta.by, 0.01)
 eff <- lower <- upper <- matrix(NA, length(gamma ), length(delta ))

 for (i in 1:length(gamma )){
	for (j in 1:length(delta )){
 	eff[i,j] <- point["delta1"]-gamma[i]*delta[j]
	lower[i,j] <- ACME1.95[1] -gamma[i]*delta[j]
	upper[i,j] <- ACME1.95[2] -gamma[i]*delta[j]
	}
	}

 contour(gamma,delta , eff,levels=0, lwd=2, main = expression(paste("A) Sensitivity of ",hat(delta)(1))),  
                     xlab = expression(gamma[wm]), ylab = expression(beta[wm]) )
 contour(gamma, delta ,lower, levels=0,lwd=1,add=TRUE,lty = 4)
 contour(gamma, delta ,upper, levels=0,lwd=1,add=TRUE,lty = 4)

#zeta0
 alpha  <- seq( -0.8+eta.by, 0-eta.by, 0.01)
 beta  <- seq( 0+eta.by, 0.8-eta.by, 0.01)
 eff <- lower <- upper<- matrix(NA, length(alpha ), length(beta ))

 for (i in 1:length(alpha )){
	for (j in 1:length(beta )){
 	eff[i,j] <- point["zeta0"]  + alpha[i]*beta[j]
	lower[i,j] <-  ANDE0.95[1]  + alpha[i]*beta[j]
	upper[i,j] <-  ANDE0.95[2]  +alpha[i]*beta[j]
	}
	}

 contour(alpha,beta , eff,levels=0, lwd=2, main = expression(paste("B) Sensitivity of ",hat(zeta)(0))),  
                     xlab = expression(gamma[wm]), ylab = expression(beta[wm]) )
 contour(alpha, beta ,lower, levels=0,lwd=1,add=TRUE,lty = 4)
 contour(alpha, beta ,upper, levels=0,lwd=1,add=TRUE,lty = 4)

#delta w1
 alpha  <- seq( 0+eta.by, 0.8-eta.by, 0.01)
 beta  <- seq( 0+eta.by, 0.8-eta.by, 0.01)
 eff <- lower <- upper<- matrix(NA, length(alpha ), length(beta ))

 for (i in 1:length(alpha )){
	for (j in 1:length(beta )){
 	eff[i,j] <- point["deltaw1"]-alpha[i]*beta[j]
	lower[i,j] <- ACMEw1.95[1] -alpha[i]*beta[j]
	upper[i,j] <- ACMEw1.95[2] -alpha[i]*beta[j]
	}
	}

 contour(alpha,beta , eff,levels=0, lwd=2, main = expression(paste("C) Sensitivity of ",hat(delta[W])(1))),  
                    xlab = expression(gamma[w]), ylab = expression(beta[w]) )
 contour(alpha, beta ,lower, levels=0,lwd=1,add=TRUE,lty = 4)
 contour(alpha, beta ,upper, levels=0,lwd=1,add=TRUE,lty = 4)

 
#delta m1 when there is positive overall confounding 
 alpha  <- seq( -0.8+eta.by, 0-eta.by, 0.01)
 beta  <- seq( 0+eta.by, 0.8-eta.by, 0.01)
 eff <- lower <- upper<- matrix(NA, length(alpha ), length(beta ))

 for (i in 1:length(alpha )){
	for (j in 1:length(beta )){
 	eff[i,j] <- point["deltam1"] - 0.02 + alpha[i]*beta[j]
	lower[i,j] <-  ACMEm1.95[1] - 0.02 + alpha[i]*beta[j]
	upper[i,j] <-  ACMEm1.95[2] - 0.02 +alpha[i]*beta[j]
	}
	}

 contour(alpha,beta , eff,levels=0, lwd=2, main = expression(paste("D) Sensitivity of ",hat(delta[M])(1))),  
                     xlab = expression(gamma[w]), ylab = expression(beta[w]) )
 contour(alpha, beta ,lower, levels=0,lwd=1,add=TRUE,lty = 4)
 contour(alpha, beta ,upper, levels=0,lwd=1,add=TRUE,lty = 4)

#deltar1
 eta.by =0.05
 gamma  <- seq( 0+eta.by, 0.8-eta.by, 0.01)
 delta  <- seq( 0+eta.by, 0.8-eta.by, 0.01)
 eff <- lower <- upper <- matrix(NA, length(gamma ), length(delta ))

 for (i in 1:length(gamma )){
	for (j in 1:length(delta )){
 	eff[i,j] <- point["deltar1"]-gamma[i]*delta[j]
	lower[i,j] <- ACMEr1.95[1] -gamma[i]*delta[j]
	upper[i,j] <- ACMEr1.95[2] -gamma[i]*delta[j]
	}
	}

 contour(gamma,delta , eff,levels=0, lwd=2, main = expression(paste("A) Sensitivity of ",hat(delta[R])(1))),  
                     xlab = expression(gamma[wm]), ylab = expression(beta[wm]) )
 contour(gamma, delta ,lower, levels=0,lwd=1,add=TRUE,lty = 4)
 contour(gamma, delta ,upper, levels=0,lwd=1,add=TRUE,lty = 4)

#zetar0
 alpha  <- seq( -0.8+eta.by, 0-eta.by, 0.01)
 beta  <- seq( 0+eta.by, 0.8-eta.by, 0.01)
 eff <- lower <- upper<- matrix(NA, length(alpha ), length(beta ))

 for (i in 1:length(alpha )){
	for (j in 1:length(beta )){
 	eff[i,j] <- point["zetar0"]  + alpha[i]*beta[j]
	lower[i,j] <-  ANDEr0.95[1]  + alpha[i]*beta[j]
	upper[i,j] <-  ANDEr0.95[2]  +alpha[i]*beta[j]
	}
	}

 contour(alpha,beta , eff,levels=0, lwd=2, main = expression(paste("B) Sensitivity of ",hat(zeta[R])(0))),  
                     xlab = expression(gamma[wm]), ylab = expression(beta[wm]) )
 contour(alpha, beta ,lower, levels=0,lwd=1,add=TRUE,lty = 4)
 contour(alpha, beta ,upper, levels=0,lwd=1,add=TRUE,lty = 4)


