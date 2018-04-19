# Day3.R
# This is a script that is a continuation of the Introduction to Statistics using R
# Purpose: To learn about the different types of data distributions and to go over t-tests
# Demi-Leigh
# 17 April 2018.

# Discrete Distributions --------------------------------------------------

# Bernoulli distribution
  # Toss a coin once. 
    # There are only two options, heads or tails
      # Nothing else
       # If the coin is fully balanced then their is a 50% probablity choice for either side

# Binomial distribution
  # Results from repeating identical experiments 
    # that produce a binary outcome with probability
      # it represents a collection of Bernoulli trials

# Negative binomial distribution
  # Counts the number of successes in a sequence 
    # of independent Bernoulli trials with probability
  # Could for example be used to predict the number of heads 
    # that result from a series of coin tosses

# Geometric distribution
  # The geometric distribution is useful to model the number of times a die 
    # must be tossed in order for a six to be observed

# Poisson distribution
  # Fixed interval of time which you count the number of occurences
    # Amount of event within a fixed time


# Continuous Distributions ------------------------------------------------

# Normal distribution
  # Gaussian distribution
  # Independent and identically distributed random variables 
  # Mean, stdev, etc. (Descriptors of central tendency) makes sense from these figures
  # Most samples will be clustered around the mean 

# Uniform distribution
  # Rectangular distribution
    # All measurements of the same magnitude included with this distribution 
      # are equally probable
        # basically random numbers
  # Every interval within the range has an even or equal probablility 

# Uniform distrubtion
  # Not in biology
  # Generate random homogenous populations

# Student T distribution
  # Calculate mean from a bunch of samples

# Chi-squared distribution
  # Statistical properties
  # Linear models - statistical differences

# Exponential distribution
  # Time expended or elapses between events
    # in a Poisson point process

# F distribution
  # Theoretical -  Compare a bunch of variances

# Gamma distribution
  # Skewed towards the right

# Beta distribution
  # Do not come across often in biology

## Transform Data oif it does not conform ##


# Libraries required to find one's data distribution ----------------------

library(fitdistrplus)
library(logspline)

# Cullen and Frey graph
  # See where dot appears 
  # based on its location - suggestion of which distribution our data belongs to

# Generate Cullen and Frey graph ------------------------------------------

# Generating normal Data

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

# Visualising the Distribution of the Data

hist(r_norm)
descdist(r_norm, discrete =  FALSE, boot = 100)


# Uniformly distributed data

y <- runif(100)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)


# T-tests -----------------------------------------------------------------

# T-test - two things
# ANOVA -  more two things


# Load libraries ----------------------------------------------------------

library(tidyverse)

## Assumptions

# the dependent variable must be continuous
  # the observations in the groups being compared are independent of each other
    # the data are normally distributed, and
      # the data are homoscedastic (there are no outliers)

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))



# Check assumptions -------------------------------------------------------

# Normality
# For this we may use the Shapiro-Wilk Test
# Hypothesis - not significantly different from normal 
  # less than 0.05 = less than normal 
    # Make sure p is not less than 0.05
shapiro.test(r_dat$dat)
shapiro.test(r_dat$dat)[1] # Only output the p value
shapiro.test(r_dat$dat)[2] # Only output the p value

# But that is testing all f the data together
# We must be abit more clever about how we make this test

# This is how we test the normality of the data:
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))

# Remember, the data are normal when p > 0.05
# the data are non-normal when p < = 0.05


# Check homoscedasticity --------------------------------------------------
# There are many ways to check for homoscedasticity
# which is the similarity of variance between sample sets
# For now we will simply say that this assumptions is met when
# the variance of the samples are not more than 2 - 4 times greater
# than one another

# Check everything at once ...
# WRONG
var(r_dat$dat)

# Or do it the tidy way
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))


# A one sample t-test -----------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5), sample("A"))
                    
 r_one      
 
# Test normality
shapiro.test(r_one$dat)

# Perhaps visualisation?
ggplot(data = r_one, aes(x = dat)) +
  geom_density()

# Run the test
t.test(r_one$dat, mu = 20)

# Run a test we know will produce a significant result
t.test(r_one$dat, mu = 30)


# Pick a side -------------------------------------------------------------

# Are these data SMALLER/LESS than the population mean
t.test(r_one$dat, mu = 20, alternative = "less")

#Or GREATER
t.test(r_one$dat, mu = 20, alternative = "greater")

# But what about for the larger population mean?
# Are the samples less than the population of 30?
t.test(r_one$dat, mu = 30, alternative = "less")

# What about greater than?
t.test(r_one$dat, mu = 30, alternative = "greater")


# Two-sample t-tests ------------------------------------------------------

# Random normal data
# Create another dataframe
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# Run a default/basic test
# ~ what are the data you are comparing and what is the category? 
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# Pick a side
# Is A less than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")

# Is A greater than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")


# A t-test workflow -------------------------------------------------------

# Load libraries

library(ggpubr)

# Loading the Data

ecklonia <- read_csv("ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

# Visualizing the Data

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# Formulating a hypothesis

  # Filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

  # Create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Checking assumptions

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(variable.norm = as.numeric(shapiro.test(value)[2]),
            variable_var = var(value))

# Running an analysis

  # Traditional output
t.test(value ~ site, data = ecklonia_sub, 
       var.equal = TRUE, alternative = "greater")
  
# Dataframe output
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", 
              var.equal = TRUE, alternative = "greater")



# Exercise with the ecklonia dataset --------------------------------------

# Loading the Data

eckl <- read_csv("ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

# Visualizing the Data

ggplot(data = eckl, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# Filter the data
eckl_sub <- eckl %>% 
  filter(variable == "frond_mass")

# Create a new figure
ggplot(data = eckl_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "frond mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Checking assumptions

eckl_sub %>% 
  group_by(site) %>% 
  summarise(variable.norm = as.numeric(shapiro.test(value)[2]),
            variable_var = var(value))

# Running an analysis

t.test(value ~ site, data = eckl_sub, 
       var.equal = TRUE, alternative = "greater")

compare_means(value ~ site, data = eckl_sub, method = "t.test", 
              var.equal = TRUE, alternative = "greater")


# Exercise 1 --------------------------------------------------------------

# Generating normal Data
r_norm <- data.frame(dat = c(rnorm(n = 1000, mean = 25, sd = 1),
                            rnorm(n = 1000, mean = 15, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Checking the Normality of the data
r_norm %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))

# A basic test
t.test(dat ~ sample, data = r_norm, var.equal = TRUE)

# Is A less than B?
t.test(dat ~ sample, data = r_norm, var.equal = TRUE, alternative = "less")

# Is A greater than B?
t.test(dat ~ sample, data = r_norm, var.equal = TRUE, alternative = "greater")

# Visualisation
ggplot(data = r_norm, aes(x = dat)) +
  geom_density(aes(fill = sample))

ggplot(data = r_norm, aes(x = dat, y = dat, fill = sample)) +
  geom_boxplot()





