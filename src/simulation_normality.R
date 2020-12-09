########################################
# Asymptotic Normality of 
# Gamma MLE on scale given shape
########################################

####################
# Setup
####################

# set parameters
d = 2
theta0 = 2

# function that computes MLE of scale parameter in Gamma
mle_scale <- function(x){
  return(mean(x)/d)
}

# function that computes Fisher information for scale parameter in Gamma
fisher <- function(){
  return(-d/theta0^2 + (2/theta0^3)*d*theta0)
}

####################
# Simulation
####################

# number of trials
N = 1000

# arrays to store mles
mle10 = rep(0,N)
mle40 = rep(0,N)
mle70 = rep(0,N)
mle100 = rep(0,N)

set.seed(1)

# main loop
for (i in 1:N){
  print(i)
  original_x = rexp(n=100, rate=1/theta0) + rexp(n=100, rate=1/theta0)
  
  x = original_x
  mle = mle_scale(x)
  mle100[i] = mle
  
  x = original_x[1:70]
  mle = mle_scale(x)
  mle70[i] = mle
  
  x = original_x[1:40]
  mle = mle_scale(x)
  mle40[i] = mle

  x = original_x[1:10]
  mle = mle_scale(x)
  mle10[i] = mle
}

# saving plot for N=10
x_seq = seq(0, 4, length = N)
y_seq = dnorm(x_seq, mean=theta0, sd=sqrt(1/(10*fisher())))

png("../misc/N10.jpg", width = 300, height = 300)
hist(mle10, prob=TRUE, ylim=c(0,3), xlim=c(0,4),
     xlab="MLEs",
     main="Histogram of MLE, N=10")
lines(x=x_seq, y=y_seq, col="red")
dev.off()

# saving plot for N=40
y_seq = dnorm(x_seq, mean=theta0, sd=sqrt(1/(40*fisher())))

png("../misc/N40.jpg", width = 300, height = 300)
hist(mle40, prob=TRUE, ylim=c(0,3), xlim=c(0,4),
     xlab="MLEs",
     main="Histogram of MLE, N=40")
lines(x=x_seq, y=y_seq, col="red")
dev.off()

# saving plot for N=70
y_seq = dnorm(x_seq, mean=theta0, sd=sqrt(1/(70*fisher())))

png("../misc/N70.jpg", width = 300, height = 300)
hist(mle70, prob=TRUE, ylim=c(0,3), xlim=c(0,4),
     xlab="MLEs",
     main="Histogram of MLE, N=70")
lines(x=x_seq, y=y_seq, col="red")
dev.off()

# saving plot for N=100
y_seq = dnorm(x_seq, mean=theta0, sd=sqrt(1/(100*fisher())))

png("../misc/N100.jpg", width = 300, height = 300)
hist(mle100, prob=TRUE, ylim=c(0,3), xlim=c(0,4),
     xlab="MLEs",
     main="Histogram of MLE, N=100")
lines(x=x_seq, y=y_seq, col="red")
dev.off()