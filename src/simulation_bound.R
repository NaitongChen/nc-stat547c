########################################
# Normal Approximation of 
# Gamma MLE on scale given shape
########################################

####################
# Setup
####################

# set parameters
p = 1
d = 2
theta0 = 2

# function h (section 3.3.3 in Anastasiou and Ley)
h <- function(x){
  return(1/(x^2 + 2))
}
h_norm = 0.5
hp_norm = 3*sqrt(1.5)/16
E_hz = 0.5*exp(1)*sqrt(pi)*2*pnorm(-sqrt(2)*1)

# h <- function(x){
#   return(-1/(x^2 + 2))
# }
# h_norm = 0.5
# hp_norm = 3*sqrt(1.5)/16
# E_hz = -0.5*exp(1)*sqrt(pi)*2*pnorm(-sqrt(2)*1)


# function that computes the bound (eq 3.2 in Anastasiou and Ley)
bound <- function(n){
  val = (hp_norm/sqrt(n))*(2+(3+6*(p/d))^(3/4)) +
    (d != 1 || p != 1)*(1-2*(p/(n*d))^(1/p)*(exp(lgamma((n*d+1)/p) - (lgamma(n*d/p)))) + 
                          (p/(n*d))^(2/p)*(exp(lgamma((n*d+2)/p) - (lgamma((n*d)/p))))) *
    (8*h_norm + (hp_norm*sqrt(n*d*p)*abs(p-1))/2 * ((p<2)/(2^(p-2)) + (p>=2)*(3/2)^(p-2)))
  return(val)
}

# function that computes MLE of scale parameter in Gamma
mle_scale <- function(x){
  return(mean(x)/d)
}

# function that computes Fisher information for scale parameter in Gamma
fisher <- function(){
  return(-d/theta0^2 + (2/theta0^3)*d*theta0)
}

# function that computes distance of one sample
dist <- function(that, n){
  val = h( sqrt(n * fisher()) * (that - theta0) )
  return(val)
}

####################
# Simulation
####################

# 10000 trials of n = 10, 100, 1000, 10000, 100000
N = 10000

# arrays that hold the simulation results
r10 = rep(0, N)
r100 = rep(0, N)
r1000 = rep(0, N)
r10000 = rep(0, N)
r100000 = rep(0, N)

# arrays that hold the mles
mle10 = rep(0, N)
mle100 = rep(0, N)
mle1000 = rep(0, N)
mle10000 = rep(0, N)
mle100000 = rep(0, N)

set.seed(1)

# main loop
for (i in 1:N){
  print(i)
  original_x = rexp(n=100000, rate=1/theta0) + rexp(n=100000, rate=1/theta0)
  
  x = original_x
  n = length(x)
  mle = mle_scale(x)
  mle100000[i] = mle
  r100000[i] = dist(mle, n)
  
  x = original_x[1:10000]
  n = length(x)
  mle = mle_scale(x)
  mle10000[i] = mle
  r10000[i] = dist(mle, n)
  
  x = original_x[1:1000]
  n = length(x)
  mle = mle_scale(x)
  mle1000[i] = mle
  r1000[i] = dist(mle, n)

  x = original_x[1:100]
  n = length(x)
  mle = mle_scale(x)
  mle100[i] = mle
  r100[i] = dist(mle, n)

  x = original_x[1:10]
  n = length(x)
  mle = mle_scale(x)
  mle10[i] = mle
  r10[i] = dist(mle, n)
}

# mean distances to be plotted/compared
dist10 = abs(mean(r10) - E_hz)
dist100 = abs(mean(r100) - E_hz)
dist1000 = abs(mean(r1000) - E_hz)
dist10000 = abs(mean(r10000) - E_hz)
dist100000 = abs(mean(r100000) - E_hz)

# bounds of corresponding sample sizes
bound10 = bound(10)
bound100 = bound(100)
bound1000 = bound(1000)
bound10000 = bound(100000)
bound100000 = bound(1000000)

# priting statements to check the results
c(abs(theta0 - mean(mle10)), abs(theta0 - mean(mle100)), 
  abs(theta0 - mean(mle1000)), abs(theta0 - mean(mle10000)), abs(theta0 - mean(mle100000)))
c(mean(r10), mean(r100), mean(r1000), mean(r10000), mean(r100000))

# stroing output plot
png("../misc/rplot.jpg", width = 400, height = 400)
plot(x=c(10, 100, 1000, 10000, 100000), y=c(dist10, dist100, dist1000, dist10000, dist100000), ylim=c(0.0001,1), log="xy",
     main="Sample Size vs Error", xlab="Sample Size", ylab="Error", type="b")
points(x=c(10, 100, 1000, 10000, 100000), y=c(bound10, bound100, bound1000, bound10000, bound100000), col="red", type="b")

legend("topright", 
       legend = c("Error", "Bound"), 
       col = c("black", "red"), 
       pch = c(1,1), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F)
dev.off()