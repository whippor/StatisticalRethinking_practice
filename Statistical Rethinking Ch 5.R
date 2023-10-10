#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Statistical Rethinking - Chapter 5                                          ##
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

# categorical variables in the model

# accounting for sex in the height-weight data

# DAG

# H -> W
# ^ / 
# |/
# S

# you can ignore unseen influences on each variable, 
# UNLESS the unseen variable influences more than
# one of the observed variables

# S = 1 female; S = 2 male
sim_HW <- function(S,b,a) { 
  N <- length(S)
  H <- ifelse(S==1, 150,160) + rnorm(N,0,5) # if female, mean height 150 + gaussian noise
  W <- a[S] + b[S]*H + rnorm(N,0,5)
  data.frame(S,H,W)
}
# b = proportionality of height that is weight
# a = direct effect of sex on weight

S <- rbern(100) +1
dat <- sim_HW(S, b = c(0.5,0.6), a = c(0,0)) # no sex effect, different slopes
head(dat)

########################
# workflow 1) question #
#######################+

# What is the causal effect of height on weight?
# What is the causal effect of sex on weight? (direct and indirect)
# What is the direct causal effect of sex on weight?

# how to code for categorical variables?
# 1) indicator variables (0/1)
# 2) index variables: 1, 2, 3...

###############################
# workflow 2) sketch of model #
##############################+

# DAG

# H -> W
# ^ / 
# |/
# S


################################
# workflow 3) generative model #
###############################+

# Wi ~ Normal(mui, sd)
# mui = aS[i]
# aj ~ Normal(60,10)
# sd ~ Uniform(0,10)

# simulate weights from height and sex
# S = 1 female; S = 2 male
sim_HW <- function(S,b,a) { 
  N <- length(S)
  H <- ifelse(S==1, 150,160) + rnorm(N,0,5) # if female, mean height 150 + gaussian noise
  W <- a[S] + b[S]*H + rnorm(N,0,5)
  data.frame(S,H,W)
}

# simulate two samples, all female, all male
# difference in weight is total causal effect of sex on weight

# female sample
S <- rep(1,100)
simF <- sim_HW(S,b=c(0.5,0.6), a = c(0,0))
# male sample
S <- rep(2,100)
simM <- sim_HW(S, b = c(0.5, 0.6), a = c(0,0))

# effect of sex (male-female)
mean(simM$W - simF$W)

# run data on mixed sample women and men
S <- rbern(100) + 1
dat <- sim_HW(S, b = c(0.5,0.6), a= c(0,0))


##################################
# workflow 4) build an estimator #
#################################+

# estimate posterior
m_SW <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(60,10),
    sigma ~ dunif(0,10)),
  data = dat)

######################################
# workflow 5) validate the estimator #
#####################################+

precis(m_SW, depth = 2)  
# difference in alpha (mean weight) is total direct/indirect effect of sex
# it should be close to your all female/male difference in weight from above

##########################################
# workflow 6) run estimator on real data #
#########################################+

data("Howell1")
d <- Howell1
d <- Howell1 %>%
  filter(age >= 18)
dat <- list(
  W = d$weight,
  S = d$male + 1) # S=1 female, S=2 male

m_SW <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a[S],
    a[S] ~ dnorm(60,10),
    sigma ~ dunif(0,10)),
  data = dat)

# posterior distribution of mean W 
# NOT A FIGURE OF THE WEIGHT DISTRIBUTIONS, ONLY THE MEANS
# the model generated a bunch of means (alpha), and these
# are those mean values plotted for density
post <- extract.samples(m_SW)
dens(post$a[,1], xlim=c(39,50), lwd = 3, col = 2, 
     xlab = "posterior mean weight (kg)")
dens(post$a[,2], lwd = 3, col = 4, add = TRUE)

# posterior distributions of weight
# a simulation of the weight distributions given as density
# need alpha and sd to simulate, gives variance around means
W1 <- rnorm(1000, post$a[,1], post$sigma)
W2 <- rnorm(1000, post$a[,2], post$sigma)
dens(W1, xlim=c(20,70), ylim = c(0,0.085), lwd = 3, col = 2)
dens(W2, lwd = 3, col = 4, add = TRUE)

# but we still can't say anything about how much they are different
# need to compute the CONTRAST between the two. Overlap/no overlap
# of the distributions won't tell you what the contrast is between
# them, i.e.- how different are they from each other
# the CONTRAST is the estimate we want!

# causal contrast (in means)
mu_contrast <- post$a[,2] - post$a[,1]

dens(mu_contrast, xlim = c(3,10), lwd = 3, col = 1, 
     xlab = "posterior mean weight contrast (kg)")

# causal contrast between individuals

W1 <- rnorm(1000, post$a[,1], post$sigma)
W2 <- rnorm(1000, post$a[,2], post$sigma)

# contrast
W_contrast <- W2 - W1
dens(W_contrast, xlim=c(-25,35), lwd = 3, col = 1,
     xlab = "posterior weight contrast (kg")

# proportion above zero
sum(W_contrast > 0) / 1000
# proportion below zero
sum(W_contrast < 0) / 1000
# tells you - if you randomly select one man and one woman from the 
# population, how often will the man be heavier than the woman
# or lighter than the woman?
# 82% heavier
# 18% lighter

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####
