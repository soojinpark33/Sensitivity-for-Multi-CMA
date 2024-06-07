multicma <- function(x, Y.fit, M.fit, T.fit, treat, mediator1, mediator2){

########################
# Mediator w prediction#
########################
# Calculated w(1) and w(0)
dat_w1 <- dat_w0 <- dat_w <- model.frame(W.fit)
dat_w1[,treat] <-1
dat_w0[,treat] <-0
n <- nrow(dat_w)


########################
# Mediator m prediction#
########################
# Calculated m(1), m(0), m(10) and m(01)
errorw <- rnorm(n, mean=0, sd=summary(W.fit)$sigma) 
errorm <- rnorm(n, mean=0, sd=summary(M.fit)$sigma) 
dat_m1 <- dat_m0 <- dat_m10 <- dat_m01 <- model.frame(M.fit)
dat_m1[,treat] <- dat_m10[,treat] <- 1
dat_m0[,treat] <- dat_m01[,treat] <- 0
dat_m0[,mediator1] <- dat_m10[,mediator1] <- predict (W.fit, newdata=dat_w0) + errorw
dat_m1[,mediator1] <- dat_m01[,mediator1] <- predict (W.fit, newdata=dat_w1) + errorw

########################
# Outcome prediction   #
########################
# create data in which T=1 and T=0 
dat_1 <- dat_0 <- dat_1000 <- dat_1010 <- dat_1100 <- dat_1101 <-model.frame(Y.fit)
dat_1[,treat] <- dat_1000[,treat] <- dat_1010[,treat] <- dat_1100[,treat] <-dat_1101[,treat] <- 1
dat_0[,treat] <-0

dat_1[,mediator1] <- dat_1100[,mediator1] <- dat_1101[,mediator1] <- predict (W.fit, newdata=dat_w1) + errorw
dat_0[,mediator1] <- dat_1000[,mediator1] <- dat_1010[,mediator1]  <- predict (W.fit, newdata=dat_w0) + errorw

dat_1[,mediator2] <- predict (M.fit, newdata=dat_m1) + errorm
dat_0[,mediator2] <- dat_1000[,mediator2] <- dat_1100[,mediator2]<- predict (M.fit, newdata=dat_m0) + errorm
dat_1010[,mediator2] <- predict (M.fit, newdata=dat_m10) + errorm
dat_1101[,mediator2] <- predict (M.fit, newdata=dat_m01) + errorm

y1 <- predict (Y.fit, newdata=dat_1)
y0 <- predict (Y.fit, newdata=dat_0)
y1000 <- predict (Y.fit, newdata=dat_1000) 
y1010 <- predict (Y.fit, newdata=dat_1010) 
y1100 <- predict (Y.fit, newdata=dat_1100) 
y1101 <- predict (Y.fit, newdata=dat_1101) 

#Calculate avg. direct/indirect effects
delta1 <- mean(y1 - y1000)
deltaw_1 <- mean(y1 -y1010) 
deltam_1 <- mean(y1010 -y1000)
zeta0 <- mean(y1000 - y0)
deltar_1 <- mean(y1- y1100)
zetar_0 <- mean(y1100-y0)
tau <-mean(y1-y0)

return(c(delta1, deltaw_1, deltam_1, zeta0,tau, deltar_1, zetar_0))
}

# Bootstrap CI
cma.sense.b <- function(x, Y.fit, M.fit, W.fit, treat, mediator1, mediator2){
  data <- model.frame(Y.fit)
  sb <- sample(1:nrow(data), nrow(data), repl=TRUE)
  Data.b <- data[sb,]

  Y.fit.b <- update(Y.fit, data=Data.b)
  M.fit.b <- update(M.fit, data=Data.b)
  W.fit.b <- update(W.fit, data=Data.b)
  out <- multicma(NULL, Y.fit.b, M.fit.b, W.fit.b, treat, mediator1, mediator2)
  return(out)
}

	
