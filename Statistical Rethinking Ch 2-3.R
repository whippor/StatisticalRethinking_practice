#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Statistical Rethinking - Chapter 2 - 3                                      ##
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

# 2.1 The garden of forking data ####


# 2.1.3 How to calculate the plausibility of an outcome based on prior information

# given 4 values: some combination of 0 and 1, what is the actual combination?
# you randomly draw (with replacement) three values and get: 1, 0, 1
# 0, 0, 0, 0 = 0 ways to get observation
# 1, 0, 0, 0 = 3 ways 
# 1, 1, 0, 0 = 8 ways
# 1, 1, 1, 0 = 9 ways
# 1, 1, 1, 1 = 0 ways

# You can update predictions of what the combination of values are by making a
# new draw of the same type of data. An additional draw produces: 1, now
# multiply the number of ways to draw 1's for each possible combination [W new]
# by prior observations [W prior].
# 0, 0, 0, 0 = 0 [D prior] * 0 [D new] = 0 
# 1, 0, 0, 0 = 3 [D prior] * 1 [D new] = 3
# 1, 1, 0, 0 = 8 [D prior] * 2 [D new] = 16
# 1, 1, 1, 0 = 9 [D prior] * 3 [D new] = 27
# 1, 1, 1, 1 = 0 [D prior] * 0 [D new] = 0

# You can also update predictions with diffrent types of information. Say that
# you are told that a set with a single '1' is 3x as likely as three '1's and
# that two '1's are twice as likely as three '1's. Again, you can multiply by
# the W prior.
# 0, 0, 0, 0 = 0  [D prior] * 0 [known likelihood] = 0 
# 1, 0, 0, 0 = 3  [D prior] * 3 [known likelihood] = 9
# 1, 1, 0, 0 = 16 [D prior] * 2 [known likelihood] = 32 
# 1, 1, 1, 0 = 27 [D prior] * 1 [known likelihood] = 27
# 1, 1, 1, 1 = 0  [D prior] * 0 [known likelihood] = 0

# 2.1.3 From counts to probablility

# To define probability of drawing a '1', you need to define the proportion of
# numbers that are '1' in each combination.
# 0, 0, 0, 0 = 0 
# 1, 0, 0, 0 = 0.25 
# 1, 1, 0, 0 = 0.50
# 1, 1, 1, 0 = 0.75
# 1, 1, 1, 1 = 1
# And given:
# D new = 1, 0, 1

# plausibility of p after D new ∝ ways p can produce D new × prior plausibility
# of p

# Standardize so that values are between 1 and 0 by dividing by sum of products.

# plausibility of p after D new = (ways p can produce D new × prior plausibility
# of p) / sum of products

# example: if there are 5 possible initial states, and there are 'ways' number
# of ways to get the resulting data from each state, you can calculate the
# plausibility of each by dividing each way by the sum of ways.

ways <- c(0, 3, 8, 9, 0)
ways/sum(ways)
# [1] 0.00 0.15 0.40 0.45 0.00 
# these represent the posterior probabilities given the new information


# R code 2.1: Calculating the probabilities that a globe
# has either 0, 25, 50, 75, or 100% water after taking
# randomized observations of a place on the globe and 
# finding either Water "W" or Land "L"

# the hypothetical sample you collect
sample <- c("W", "L", "W", "W", "W", "L", "W", "L", "W")

# total count of Water observed
W <- sum(sample == "W") 

# total count of water observed
L <- sum(sample == "L") # of L observed

# list of possible proprotions of Water on the globe 
# (simplified for example as only having 0 - 100%
# in 25% increments)
p <- c(0, 0.25, 0.5, 0.75, 1) 

# the number of 'ways' that the observed data could have
# been seen (forking garden) using sapply to apply
# a function to the vector p
# 4 is used because there are 4 paths that branch from
# each possibility for choice of Water or Land
# LLLL = 0% Water
# WLLL = 25% Water
# WWLL = 50% Water
# WWWL = 75% Water
# WWWW = 100% Water
ways <- sapply(p, function(q) (q*4)^W * ((1-q)*4)^L)

# Take the counts of ways and standardize them to
# probabilities (a collection of positive real numbers 
# between 0 and 1 that add up to 1, reduces potential huge 
# numbers for large samples)
prob <- ways/sum(ways)

# put all results together into table
cbind(p, ways, prob)

###############

# Generative model: make a function that will make a sample
# data set from a globe covered in p water N times
sim_globe <- function( p = 0.7, N = 9 ) {
  sample(c("W", "L"), size = N, prob = c(p, 1-p), replace = TRUE)
}

sim_globe()

# test the simulation on extreme values where you should know the 
# answer to see if it behaves properly
sim_globe(p = 1, N = 20) # should produce all "W"s

# also, run the simulation many times and the result should
# approach whatever you dictate p is
sum(sim_globe(p = 0.5, N = 1e4) == "W") / 1e4

###############

# Function to calculate the posterior distribution (breaks with N in the low 380s)
compute_posterior <- function( the_sample, poss = c(0, 0.25, 0.5, 0.75, 1))
{
  W <- sum(the_sample == "W") # number of W observed
  L <- sum(the_sample == "L") # number of L observed
  ways <- sapply(poss, function(q) (q*(length(poss - 1)))^W * ((1-q)*(length(poss - 1)))^L)
  post <- ways/sum(ways)
  bars <- sapply(post, function(q) make_bar(q))
  data.frame( poss, ways, post = round(post, 3), bars)
}

sample <- sim_globe(p = 0.35, N = 100)
compute_posterior(sample, poss = seq( from = 0, to = 1, by = 0.1))

###############

# using grid approximation to generate posterior distribution
# uses points along a continuum of probability and 'connects the dots'

z <- 1000
# define grid (how fine scale?)
p_grid <- seq(from = 0, to = 1, length.out = z)

# define prior (choose 1)
prior <- rep(1, z)

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd_posterior <- likelihood * prior

# standardize the posterior so it sums to 1
posterior <- unstd_posterior / sum(unstd_posterior)

# draw samples from the posterior
samples <- sample(p_grid, prob = posterior, size = 1e4, replace= TRUE)

# visualize the draws
plot(samples)

# plot the density estimate from the samples
dens(samples)

# compare to actual posterior distribution
plot(p_grid, posterior, type = "b", xlab = "probability of water",
     ylab = "posterior probability")
mtext(paste(sum(z), "points"))

# typically you want to summarize the results from your posterior:
# 1) intervals of defined boundaries
# 2) questions about intervals of defined probability mass
# 3) questions about point estimates

# 1) intervals of defined boundaries
# add up posterior probs where p < 0.5
sum(posterior[p_grid < 0.5])
# [1] 0.1718746
# ~17% of the posterior is below 0.5
# compare to the samples drawn below 0.5 (sum and divide by # of samples)
sum(samples < 0.5)/ 1e4
# [1] 0.1653
# different because samples drawn are not exactly the 'ideal' distribution created
# proportion between .5 and .75
sum(samples > 0.5 & samples < 0.75) /1e4
# [1] 0.6017

# 2) invervals of defined mass
# ? where is the boundary for the lower 80% of data? (easier to run on samples)
quantile(samples, 0.8)
#       80% 
# 0.7617618
# samples between the 10th and 90th percentiles
quantile(samples, c(0.1, 0.9))
#       10%       90% 
# 0.4494494 0.8128128 
# Describing intervals in this way becomes difficult when the distribution
# is asymmetrical. For example, if we get three W from the 'world' example:
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)
# view the plot
plot(samples)
dens(samples)
# extract compatibility interval of middle 50% of density
PI(samples, prob = 0.5)
#       25%       75% 
# 0.7067067 0.9319319
# this interval is misleading because it excludes the most plausible values
# closer to 1 given the sample
# a better value is the Highest Posterior Density Interval (HPDI) = the narrowest
# interval containing the specified probability mass 
HPDI(samples, prob = 0.5)
# now it captures the 50% that reside within the highest probabilities
# HPDI is computationally more intensive has 'simulation variance' (it is 
# sensitive to how many samples you draw from the posterior)

# 3) point estimates (not recommended)
# what is the value with the highest probability (maximum a posteriori - MAP)?
p_grid[which.max(posterior)]
# [1] 1
# or from the samples:
chainmode(samples, adj = 0.01) # the mode
# [1] 0.9845318
mean(samples) # the mean
# [1] 0.8008744
median(samples) # the median
# [1] 0.8448448
# But which to use? A loss function can help choose. It penalizes the various
# points based on how far they are from the correct answer
# This is theoretically set at 0.5 in this example:
sum(posterior*abs(0.5-p_grid)) # gives what the expected average loss will be:
# [1] 0.3128752
# to run over all possible values use sapply:
loss <- sapply(p_grid, function(d) sum(posterior*abs(d-p_grid)))
# now the object 'loss' contains all the loss values for each decision
# find the lowest with:
p_grid[which.min(loss)]
# [1] 0.8408408 # very close to the median

###############

# using the quadratic approximation to create the posterior distribution
# (call 'quadratic', but essentially the same as 'Gaussian', uses 'hill climbing'
# algorithm to find the peak, then calculates the curve from nearby values)

globe_qa <- quap(
  alist(
    W ~ dbinom(W + L, p), # binomial likelihood
    p ~ dunif(0, 1) # uniform prior
  ),
  data = list(W = 6, L = 3) # these are the observed tosses
)
precis(globe_qa)

# knowing the posterior, use analytic calculation using the beta distribution
# to quantify how good the quadratic approximation is

# analytic curve
W <- 6
L <- 3
curve(dbeta(x, W + 1, L + 1), from = 0, to = 1)
# quadratic approximation
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE)



###############

# Sampling the posterior

# take 1000 samples from the beta distribution
post_samples <- rbeta(1e4, 6+1, 3+1)

dens(post_samples, lwd = 4, col = 2, xlab = "proportion water", adj = 0.1)
curve(dbeta(x, 6+1, 3+1), add = TRUE, lty = 2, lwd = 3)

# now simulate the posterior predictive distribution
pred_post <- sapply(post_samples, function(p) sum(sim_globe(p, 10) == "W"))
tab_post <- table(pred_post) 
plot.new()
for ( i in 0:10 ) lines(c(i,i), c(0, tab_post[i+1]), lwd = 4, col = 4)


###############

# simulator with misclassification error
# (number of samples "W" not 100% correct)
sim_globe2 <- function( p=0.7, N = 9, x = 01 ) {
  true_sample <- sample(c("W", "L"), size = N, prob = c(p, 1-p), replace = TRUE)
  obs_sample <- ifelse( runif(N) < x,
                        ifelse(true_sample == "W", "L", "W"), # error
                        true_sample) # no error
  return(obs_sample)
}
sim_globe2()

# checking how misclassification error changes the posterior
# p = true proportion, N = number of data points, x = error rate
p <- 0.9
N <- 60
x <- 0.2
sample <- sim_globe(p = p, N = N)
compute_posterior(sample, poss = seq( from = 0, to = 1, by = 0.1)) # no error
sample <- sim_globe2(p = p, N = N, x = x)
compute_posterior(sample, poss = seq( from = 0, to = 1, by = 0.1)) # error
# Any error term tends to pull the distribution toward the middle (0.5)

###############

# Markov Chain Monte Carlo MCMC is often the only viable way to calculate
# posteriors when you have lots of parameters (esp multilevel models)

n_samples <- 1000
p <- rep(NA, n_samples)
p[1] <- 0.5
W <- 6
L <- 3
for(i in 2:n_samples) {
  p_new <- rnorm(1, p[i - 1], 0.1)
  if(p_new < 0) p_new <- abs(p_new)
  if(p_new > 1) p_new <- 2 - p_new
  q0 <- dbinom(W, W + L, p[i - 1])
  q1 <- dbinom(W, W + L, p_new)
  p[i] <- ifelse(runif(1) < q1/q0, p_new, p[i-1])
}

# compare MCMC with the analytical posterior
dens(p, xlim = c(0,1))
curve(dbeta(x, W + 1, L + 1), lty = 2, add = TRUE)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# END OF CHAPTER EXERCISES                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 2M1: Compute and plot the grid approximation posterior for each of the following
# sets of obeservations assuming a uniform prior for p
# (1) W, W, W
# (2) W, W, W, L
# (3) L, W, W, L, W, W, W

# run each set in the grid code below

# (1) W, W, W
W <- 3
N <- 3

# (2) W, W, W, L
W <- 3
N <- 4

# (3) L, W, W, L, W, W, W
W <- 5
N <- 7

# grid code with constant p
z <- 100
# define grid (how fine scale?)
p_grid <- seq(from = 0, to = 1, length.out = z)

# define prior (choose 1)
prior <- rep(1, z)

# compute likelihood at each value in grid
likelihood <- dbinom(W, size = N, prob = p_grid)
# dbinom(x, size, prob): x = observations of W, size = number of tosses, p = probability
# here it's unknown so 50/50

# compute product of likelihood and prior
unstd_posterior <- likelihood * prior

# standardize the posterior so it sums to 1
posterior <- unstd_posterior / sum(unstd_posterior)

# display to posterior distribution
plot(p_grid, posterior, type = "b", xlab = "probability of water",
     ylab = "posterior probability")
mtext(paste(sum(z), "points"))


## 2M2 same as above but with a prior for p that is p < 0.5 = 0 and p > 0.5 = 1

# grid code fussy prior
z <- 100
# define grid (how fine scale?)
p_grid <- seq(from = 0, to = 1, length.out = z)

# define prior (choose 1)
prior <- ifelse(p_grid < 0.5, 0, 1)

# compute likelihood at each value in grid
likelihood <- dbinom(W, size = N, prob = p_grid)
# dbinom(x, size, prob): x = observations of W, size = number of tosses, p = probability
# here it's unknown so 50/50

# compute product of likelihood and prior
unstd_posterior <- likelihood * prior

# standardize the posterior so it sums to 1
posterior <- unstd_posterior / sum(unstd_posterior)

# display to posterior distribution
plot(p_grid, posterior, type = "b", xlab = "probability of water",
     ylab = "posterior probability")
mtext(paste(sum(z), "points"))

## 2M3:
# Assume 2 globes, Earth and Mars. Earth = 70% water, Mars = 100% land.
# One globe is tossed, don't know which one, and an L is produced. 
# Probability of which globe thrown is 0.5. Show the posterior prob
# that the tossed globed was Earth conditional on seeing land
# Pr(Earth|land) is 0.23.

# Pr(land|Earth) = 1-0.7 = 0.3
# Pr(land|Mars) = 1

# Pr(Earth) = 0.5
# Pr(Mars) = 0.5

# Pr(Earth|land) = Pr(land|Earth)*Pr(Earth) / Pr(land)

# Pr(Earth|land) = Pr(land|Earth)*Pr(Earth) / Pr(land|Earth)*Pr(Earth) + Pr(land|Mars)*Pr(Mars)
# Pr(Earth|land) = 0.3 * 0.5 / 0.3 * 0.5 + 1 * 0.5
(0.3*0.5)/(0.3*0.5 + 1*0.5)
[1] 0.2307692

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

###################################################
# TIDYVERSE VERSION
library(tidyverse)
library(flextable)

# making W = 1, L = 0
d <-
  tibble(p1 = 0,
         p2 = rep(1:0, times = c(1, 3)),
         p3 = rep(1:0, times = c(2, 2)),
         p4 = rep(1:0, times = c(3, 1)),
         p5 = 1)

head(d)

# depict the possible values as a plot
d %>% 
  set_names(1:5) %>% 
  mutate(x = 1:4) %>% 
  pivot_longer(-x, names_to = "possibility") %>% 
  mutate(value = value %>% as.character()) %>% 
  
  ggplot(aes(x = x, y = possibility, fill = value)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = c("white", "navy")) +
  scale_x_discrete(NULL, breaks = NULL) +
  theme(legend.position = "none")

# ennumerate all possibilites in a table
library(flextable)

tibble(draw    = 1:3,
       marbles = 4) %>% 
  mutate(possibilities = marbles ^ draw) %>% 
  flextable()

# restructure the data to create a 'forking garden' plot
(
  d <-
    tibble(position = c((1:4^1) / 4^0, 
                        (1:4^2) / 4^1, 
                        (1:4^3) / 4^2),
           draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
           fill     = rep(c("b", "w"), times = c(1, 3)) %>% 
             rep(., times = c(4^0 + 4^1 + 4^2)))
)

# intial plot
d %>% 
  ggplot(aes(x = position, y = draw, fill = fill)) +
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values  = c("navy", "white")) +
  scale_y_continuous(breaks = 1:3) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
# these will connect the dots from the first and second draws
(
  lines_1 <-
    tibble(x    = rep(1:4, each = 4),
           xend = ((1:4^2) / 4),
           y    = 1,
           yend = 2)
)
# these will connect the dots from the second and third draws
(
  lines_2 <-
    tibble(x    = rep((1:4^2) / 4, each = 4),
           xend = (1:4^3) / (4^2),
           y    = 2,
           yend = 3)
)
# now put them all together
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_fill_manual(values  = c("navy", "white")) +
  scale_y_continuous(breaks = 1:3) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
# now center it so it looks better
d <-
  d %>% 
  mutate(denominator = ifelse(draw == 1, .5,
                              ifelse(draw == 2, .5 / 4,
                                     .5 / 4^2))) %>% 
  mutate(position = position - denominator)
(
  lines_1 <-
    lines_1 %>% 
    mutate(x    = x - 0.5,
           xend = xend - 0.5 / 4^1)
)
(
  lines_2 <-
    lines_2 %>% 
    mutate(x    = x - 0.5 / 4^1,
           xend = xend - 0.5 / 4^2)
)
# the whole thing
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_fill_manual(values  = c("navy", "white")) +
  scale_y_continuous(breaks = 1:3) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
# add polar coordinate to give it the circular feel
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 4) +
  scale_fill_manual(values = c("navy", "white")) +
  scale_x_continuous(NULL, limits = c(0, 4), breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3), breaks = NULL) +
  coord_polar() +
  theme(legend.position = "none",
        panel.grid = element_blank())

############# My old script

# 2.3 Components of the model ####

# You want to know how much water is on the earth by tossing a globe into the
# air and counting the number of times your finger lands on water 'W' or land
# 'L'.

# 2.3.2.1 Observed variables

# If we say that all tosses are independent, and that the probability of landing
# on water 'W' is the same on every toss, it fits the binomial distribution.
# So for 9 tosses that resulted in 6 'W's and 3 'L's:
dbinom(6, size = 9, prob = 0.5)
# [1] 0.1640625 
# This is the relative number of ways to get 6 water given the conditions. Same
# way that posterior probabilities for 0 and 1 were calculated above.

# 2.4.3 Grid approximation

# In order to generate priors (when you can't calculate it easily like in the
# above examples), you must generate priors that are conditioned on the data.
# One way to do this for simple examples is by Grid approximation. It is a
# numerical technique for computing posterior distributions.

# Steps for grid approximation:
# 1) define the grid (how many points? list paramter values on grid)
# 2) compute value of the prior at each parameter value on the grid
# 3) compute the likelihood at each parameter value
# 4) compute the undstandardized posterior at each parameter value: prior *
# likelihood
# 5) standardize the posterior but dividing each value by the sum of all values
# (so that all values sum to 1)

# Example (using globe tossing example above):

# define grid
p_grid <- seq(from = 0, to = 1, length.out = 100)

# define prior
prior <- rep(1, 100) # a 'flat' prior, all equal probability
# prior <- ifelse(p_grid < 0.5, 0, 1) # a prior with no values less than 0.5
#prior <- exp(-5 * abs(p_grid - 0.5)) # a prior with a sharp peak at 0.5

# compute likelihood at each value in the grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# visualize the posterior
plot(p_grid, posterior, type = "b", xlab = "probability of water", ylab = "posterior probability")
mtext("100 points")

# 2.4.4 Quadratic approximation

# Grid approximation is bad for anything that involves multiple parameters
# because they quickly inflate and require more computing power. Quadratic
# approximation allows you to identify peaks in your distribution, then apply a
# Gaussian distribution to just the peak. These peaks are often nearly 'normal',
# so the distribution applies

# Example (using globe tossing example):

library(rethinking)
globe.qa <- quap(
  alist(
    W ~ dbinom(W + L, p), # binomial likelihood
    p ~ dunif(0, 1) # uniform prior
  ),
  data = list(W = 6, L = 3)
)

# display summary of quadratic approximation

precis(globe.qa)
#   mean   sd 5.5% 94.5%
# p 0.67 0.16 0.42  0.92

# analytical calculation to validate the above approximation
W <- 6
L <- 3
curve(dbeta(x, W + 1, L + 1), from = 0, to = 1)
#quadratic approximation
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE)

# with an N = 9, the approximation isn't great (assigns positive probability to
# 100% W which we know isn't true), but as sample size increases with same ratio
# of L:W, the curve improves until it's almost the same at N = 36

# 2.4.5 Markov chain Monte Carlo

# Useful for multilevel models that would take far to long to calculate a
# posterior any other way. MCMC draws samples from the posterior and the
# histograms of these frequencies correspond to the posterior plausibilities.

# Quick example how it works:

n_samples <- 1000
p <- rep(NA, n_samples)
p[1] <- 0.5
W <- 6
L <- 3
for (i in 2:n_samples) {
  p_new <- rnorm(1, p[i-1], 0.1)
  if(p_new < 0) p_new <- abs(p_new)
  if(p_new > 1) p_new <- 2 - p_new
  q0 <- dbinom(W, W+L, p[i-1])
  q1 <- dbinom(W, W+L, p_new)
  p[i] <- ifelse(runif(1) < q1/q0, p_new, p[i-1])
}

# values of p are samples from the posterior distribution 

# validate with algorithm

dens(p ,xlim=c(0,1))
curve(dbeta(x, W+1, L+1), lty=2, add=TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CHAPTER 3                                                                    ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# There are vampires in the world! And a blood test will tell you if you are one.

# Pr(positive test result|vampire) = 0.95; given that you are a vampire, the
# chance of testing positive is 95%

# Pr(positive test result|mortal) = 0.01; given that you are mortal, the chance
# of getting a false positive is 1%

# Pr(vampire) = 0.001; vampires are rare, only 0.1% of the population.

# Suppose someone tests positive for vampirism, what is the chance that they are
# actually a vampire? Must consider the true and false detection rates.

# Pr(vampire|positive) = Pr(positive|vampire) * Pr(vampire) / Pr(positive)
# Where Pr(positive) is the average probability of testing positive.
# Pr(positive) = Pr(positive|vampire) * Pr(vampire) + 
#                Pr(positive|mortal) * (1-Pr(vampire))

Pr_Positive_Vampire <- 0.95
Pr_Positive_Mortal <- 0.01
Pr_Vampire <- 0.001
Pr_Positive <- Pr_Positive_Vampire * Pr_Vampire +
  Pr_Positive_Mortal * (1-Pr_Vampire)
(Pr_Vampire_Positive <- Pr_Positive_Vampire * Pr_Vampire / Pr_Positive)
#[1] 0.08683729; only an 8.7% chance that they are really a vampire!

# Presented in other words, suppose you are told:
# 1) in a population of 100,000 people, 100 are vampires
# 2) of the 100 who are vampires, 95 of them will test positive 
# of the 99,900 mortals, 999 of them will test positive

# Now, what proportion tested will actually be vampires?

# total positive tests = 95 + 999 = 1094, so...

# Pr(vampire|positive) = 95/1094 = 0.08683729


# 3.1 Sampling from a grid-approximate posterior ####

# Use the globe tossing example above:
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prob_p <- rep(1, 1000)
prob_data <- dbinom(6, size = 9, prob = p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

# If you draw sapmles from this posterior distribution, you will get samples in
# proportion to their probability

# Draw 10,000 samples:

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

plot(samples)
library(rethinking)
dens(samples)


# 3.2 Sampling to summarize ####

# 3.2.1 Intervals of defined boundaries

# What is the posterior probability that the proportion of water is <0.5?

# add up posterior probability where p < 0.5
sum(posterior[p_grid < 0.5])
# [1] 0.1718746

# summing the posterior in this way only works for single parameters. For
# multiple parameters you can sum the sample, then divide by number of samples.

sum(samples < 0.5) / 1e4
#[1] 0.1737

# how much posterior probability lies between 0.5 and 0.75?

sum(samples > 0.5 & samples < 0.75) / 1e4
# [1] 0.6107


# 3.2.2 Invervals of defined mass

# What is the lower bound of 80% of the data? (Compatibility interval [aka:
# Confidence interval])
quantile(samples, 0.8)
# 0.7567568; 80% of the data lies below the value 0.76

# WHere is the middle 80% of data located?
quantile(samples, c(0.1, 0.9))
# 0.4504505 0.8119119; the middle 80% of data is between 0.45 and 0.81
# these are 'percentile intervals' PI, but they aren't great for skewed data

# what if you got water on three of three tosses?
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)
plot(samples)
dens(samples)

# calculate the PI for central 50%
PI(samples, prob = 0.5)
# 0.7047047 0.9299299 
# it left out the most probable values near 1

# Instead, calculate where the 50% of highest density is with the Highest
# Posterior Density Interval (HPDI)

HPDI(samples, prob = 0.5)
# 0.8378378 0.9989990; the 50% highest density of probability lies between 84%
# and 99%








#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# HOMEWORK - WEEK 1                                                            ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1. Construct a posterior distribution for the globe exercies as if there had
# been 8 water in 15 tosses using grid approximation. Use the same flat prior.

# define grid
p_grid <- seq(from = 0, to = 1, length.out = 100)

# define prior
prior <- rep(1, 100) # a 'flat' prior, all equal probability

# compute likelihood at each value in the grid
likelihood <- dbinom(8, size = 15, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# visualize the posterior
plot(p_grid, posterior, type = "b", xlab = "probability of water", ylab = "posterior probability")
mtext("100 points")

# 2. Do again, but this time use a prior that is zero below p = 0.5 and a
# constant above p = 0.5, corresponding to the knowledge that the earth is
# mostly water. What difference does a better prior make?

# define grid
p_grid <- seq(from = 0, to = 1, length.out = 100)

# define prior
prior <- ifelse(p_grid < 0.5, 0, 1) # a prior with no values less than 0.5

# compute likelihood at each value in the grid
likelihood <- dbinom(8, size = 15, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# visualize the posterior
plot(p_grid, posterior, type = "b", xlab = "probability of water", ylab = "posterior probability")
mtext("100 points")

# 3. How would you estimate the earth's proportion of water precisely, obtaining
# the 99th percentile interval of the posterior distribution of p to be only
# 0.05 wide? This means the distance between upper and lower bound should only
# be 0.05. How many times will you have to toss the globe?