#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Statistical Rethinking - Chapter 4                                          ##
# Script created 2023-10-06                                                   ##
# Data source: Richard McElreath - Statistical Rethinking 2nd ed.             ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2023-10-10                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# FILE.csv

# Associated Scripts:
# FILE.R

# TO DO 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# IN TEXT EXERCISES                                                            +
# END OF CHAPTER EXERCISES                                                     +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


library(rethinking)
library(tidyverse)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# IN TEXT EXERCISES                                                         ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

############## LECTURES

# GENERAL WORKFLOW
# 1) state clear question
# 2) sketch causal assumptions (DAG)
# 3) create generative model
# 4) build an estimator
# 5) test the estimator against the generative model
# 6) use the estimator to test the collected data

# demographic data of !Kung people
data("Howell1")

########################
# workflow 1) question #
#######################+

# what is the association between adult weight and height?

d <- Howell1 %>%
  filter(age >= 18)

###############################
# workflow 2) sketch of model #
##############################+

# weight is some function of height
# H -> W
# W = f(H)

# keep is simple as static model (i.e.- no mechanism, only association
# between weight and height)

# but, add that there are some "unobserved" variables that influence weight
# (circle around variable indicates unobserved)

# H -> W <- (U)

# W = f(H,U)

################################
# workflow 3) generative model #
###############################+

# we will assume that weight is some proportion of height defined by b (beta)

# W = bH + U

# function to simulate weights of individuals from heights
sim_weight <- function(H,b,sd) { # sd the U portion of the data
  U <- rnorm( length(H), 0, sd) # generate U from the normal dist with mean 0 and sd
  W <- b*H + U
  return(W)
}

# generate random set of heights between some values
H <- runif(200, min = 130, max = 170)

# plug the heights into the function
W <- sim_weight(H, b = 0.5, sd = 5)

HWgenerated <- tibble(H,W)

# how to describe this generative model:
  # Wi = bHi + Ui
  # Ui ~ Normal(0,sd) # gaussian error with standard deviation sigma
  # Hi ~ Uniform(130,170) # height is uniformly distributed from 130 to 170

# where 'i' is subscript indicated individual samples
# '=' indicates deterministic relationship, '~' indicates 'distributed as'

# plot the height-weight relationship that was generated
ggplot(HWgenerated) +
  geom_point(aes(x = H, y = W))

##################################
# workflow 4) build an estimator #
#################################+

# E(Wi|Hi) = a + bHi 
# where E = average height
# a = intercept (alpha)
# b = slope (beta)

# you can use a quadratic approximation in many cases
# assumes gaussian multivariate distribution

m3.1 <-quap(
  alist(
    W ~ dnorm(mu, sigma), # Wi ~ Normal(mui, sd)
    mu <- a + b*H, # mui = a + bHi
    a ~ dnorm(0,10), # a ~ Normal(0,10)
    b ~ dunif(0,1), # b ~ Uniform(0,1)
    sigma ~ dunif(0,10) # sigma ~ Uniform(0,10)
  ) , data = list(W=W, H=H)
)

# Priors should express scientific knowledge (common sense)
# here, those would be:
# When H=0, W=0 (alpha here should be close to zero)
# Weight increases (on average) with height (slope of the line should be positive)
# Weight (kg) is less than height (cm) (slope shouldn't be excessively steep)
# sigma must be positive 

# use those assumptions to plot a bunch of possible lines, and see if any
# look 'impossible'. Change any constraints as necessary to make it more
# realistic
n <- 1e3
a <- rnorm(n,0,10) # sample our possible alphas
b <- runif(n,0,1) # sample our possible betas
plot(NULL, xlim =c(130,170), ylim =c(50,90), # plot and see
                   xlab= "height (cm)", ylab = "weight (kg)")
     for (j in 1:50 ) abline (a=a[j], b =b[j], lwd = 2, col = 2)
# the slopes don't look bad, but the intercepts give crazy values (too high
# and too low)

# IMPORTANT NOTE ON PRIOR
# THERE ARE NO CORRECT PRIORS, ONLY SCIENTIFICALLY JUSTIFIABLE PRIORS
# Justify with information outside the data - like the rest of the model
# Priors less important in simple models, but important in complex ones

######################################
# workflow 5) validate the estimator #
#####################################+

# simulate a sample of 10 people
set.seed(93)
H <- runif(10, 130, 170)
W <- sim_weight(H, b = 0.5, sd = 5)

# run the model
m3.1 <-quap(
  alist(
    W ~ dnorm(mu, sigma), # Wi ~ Normal(mui, sd)
    mu <- a + b*H, # mui = a + bHi
    a ~ dnorm(0,10), # a ~ Normal(0,10)
    b ~ dunif(0,1), # b ~ Uniform(0,1)
    sigma ~ dunif(0,10) # sigma ~ Uniform(0,10)
  ) , data = list(W=W, H=H)
)

# summary
precis(m3.1)

##########################################
# workflow 6) run estimator on real data #
#########################################+

dat <- list(W = d$weight, H = d$height) # pull out actual observations
m3.2 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b*H,
    a ~ dnorm(0,10),
    b ~ dunif(0,1),
    sigma ~ dunif(0,10)
  ), data = dat)

precis(m3.2)

# plot posterior predictive distribution against data
post <- extract.samples(m3.2)
plot(d$height, d$weight, col = 2, lwd = 3,
     xlab = "height (cm)", ylab = "weight (kg)")
for(j in 1:20)
  abline(a=post$a[j], b = post$b[j], lwd = 1)

# plot the variation around these predictions as some percentile
height_seq <- seq(130, 190, len = 20)
W_postpred <- sim( m3.2,
                   data = list(H=height_seq))
W_PI <- apply(W_postpred, 2, PI)
lines(height_seq, W_PI[1,], lty = 2, lwd = 2)
lines(height_seq, W_PI[2,], lty = 2, lwd = 2)


############## TEXT

# the utility of the gaussian distribution

# NORMAL BY ADDITION
# a coin flip by 1000 = left or right step on football field. Most people would 
# be close the middle after 16 flips. This is not specific to coin flips.
# generate random numbers between -1 and 1 that dictate direction of people
# replicate 1000 times and sum them up
pos <- replicate(1000, sum(runif(16, -1, 1)))
hist(pos)
plot(density(pos))
# this holds true for any distribution that you randomly sample from. It will
# always approach a bell curve. Here is the poisson distribution:
hist(rpois(10000, 4))
pos <- replicate(1000, sum(rpois(16, 4)))
hist(pos)
plot(density(pos))
# gamma distribution
hist(rgamma(100, shape = 3))
pos <- replicate(1000, sum(rgamma(100, shape = 3)))
hist(pos)
plot(density(pos))

# NORMAL BY MULTIPLICATION
# imagine a multiplicative relationship between 12 loci and various alleles
# that increase growth by a percentage in an organism. Sample random growth rate:
prod(1 + runif(12,0, 0.1)) # samples 12 random numbers between 1 and 1.1 and all
# 12 are multiplied together
# generate 10000 runs:
growth <- replicate(10000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)
# smaller deviation values tend to create better approximations (nearly additive)
big <- replicate(10000, prod(1+runif(12,0,0.5)))
dens(big, norm.comp = TRUE)
small <- replicate(10000, prod(1+runif(12,0,0.01)))
dens(small, norm.comp = TRUE)

# BUT large values do approximate the normal when logged :)
log_big <- replicate(10000, log(prod(1+runif(12,0,0.5))))
dens(log_big, norm.comp = TRUE)

# DEFINITION: probability distributions with discrete outcomes = probability
# mass functions (Pr). Continuous probabilty functions = probability density
# functions (p OR f)
# Tau (T) = 1/sd^2 -> used in some bayesian programming packages instead of sd

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####