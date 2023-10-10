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

# example: if there are 5 possible initial states, and there are 'ways' number
# of ways to get the resulting data from each state, you can calculate the
# plausibility of each by dividing each way by the sum of ways.

ways <- c(0, 3, 8, 9, 0)
ways/sum(ways)

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

sample <- sim_globe(p = 0.35, N = 40)
compute_posterior(sample, poss = seq( from = 0, to = 1, by = 0.1))


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

############### SUBSECTION HERE

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
