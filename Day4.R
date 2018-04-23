# Day4.R
# This is a script that is a continuation of the Introduction to Statistics using R
# Purpose: To revisit t-tests and learn about ANOVA tests
# Demi-Leigh
# 19 April 2018.


# ANOVA -------------------------------------------------------------------
# Compare the means of multiple samples
# Running muliple t-tests compounds the amount of error that could arise
# Increases the probability of rejecting the null hypothesis 

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)

# Revising t-tests --------------------------------------------------------

# Grab the data
chicks <- as_tibble(ChickWeight)

# Subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

# Perform the t-test
compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")

# t-test
t.test(weight ~ Diet, data = chicks_sub)
  # Interpretation: We do not reject the Null hypothesis 
  # because there is no particular difference between Diet 1 and Diet 2


# One-way ANOVA  ----------------------------------------------------------
# Research Question: Is there a difference in chick mass attached after 
  # 21 Days after the chickens having been fed four different Diets?
# Null Hypothesis: There is no difference in chicken mass at 21 days 
  # after having been fed one of the four diets.

chicks_21 <- chicks %>% 
  filter(Time == 21)

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

# Pr(>F) = Probability is less than 0.05
# Pr - takes mean of squares and degrees of freedom and puts it into a F-table
# We do not accept the Null hypothesis, therefore accepting the alternative hypothesis that
# there is a difference in chicken mass at 21 days after being fed one of the four diets.

# Are all diets having an effect?
# Graphical Depiction?

sub1 <- chicks_21 %>% 
  group_by(Diet) %>%
  summarise(mn.wt = mean(weight, na.rm = TRUE),
            sd.wt = sd(weight))

ggplot(sub1, aes(x = Diet, y = mn.wt)) +
  geom_point() +
  geom_errorbar(aes(ymin = mn.wt - sd.wt, ymax = mn.wt + sd.wt), width=0.5) 

# Box plot is for a distribution of many points of data
ggplot(dat = chicks_21, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE)

# Tukey HSD test ----------------------------------------------------------
TukeyHSD(chicks.aov1)
Tukey <- TukeyHSD(chicks.aov1)

# Another method to Test significance
  # If the lower confidence interval is positive then = significant difference

# Box plot
ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2)) 

# How to create a Tukey Dataframe
chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)

chicks_Tukey$pairs <- as.factor(row.names(chicks_Tukey))

ggplot(chicks_Tukey) +
  geom_segment(aes(x = pairs, xend = pairs, y = lwr, yend = upr)) +
  geom_point(aes(x = pairs, y = diff))

# Or just plot confidence intervals the base R way
# Shame

plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))


# Multiple factor ANOVA---------------------------------------------------------

# Creating a dataframe with just these days
chicks_0_21 <-ChickWeight %>% 
  filter(Time %in% c(0, 2, 21))

# Visualising the data
ggplot(data = chicks_0_21, aes(x = as.factor(Time), y = weight)) +
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))

# Run an ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Perform a Tukey post-hoc test
TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Look at the confidence intervals
plot(TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21)))

# Introduce the opportunity of making errors - multiple ANOVA's

# Look only at day 0 and 21 for both Time and Diet
summary(aov(weight ~ Diet + as.factor(Time), 
            data = filter(ChickWeight, Time %in% c(0, 21))))


# Or simply look at ALL of the Time
# ... which is NOT the hypothesis
summary(aov(weight ~ Diet + as.factor(Time), 
            data = ChickWeight))
# Note the increase in the degrees of freedom for the time fcator
# But no increase for the d.f. for Diet

# Now to look at interactions BETWEEN factors
summary(aov(weight ~ Diet * as.factor(Time), 
            data = filter(ChickWeight, Time %in% c(0, 21))))


# let's look at the Tukey Results
TukeyHSD(aov(weight ~ Diet * as.factor(Time), 
            data = filter(ChickWeight, Time %in% c(0, 21))))

# Visualising the Tukey results
plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), 
                  data = filter(ChickWeight, Time %in% c(0, 21)))))

# Create a line graph to help explain this concept
 
 # First create mean values by Time and Diet (Per Diet, Per Day)
chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = T))

  # Visualise it
ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet)) +
  geom_line(size = 2) +
  geom_point(shape = 15, size = 5)

# Non-parametric tests ----------------------------------------------------
# But what if...
# ...we don't have normal data?

# For the t-test we rather use Wilcox rank sum test
wilcox.test() # And then one fills in the same as for t.test()

# And now for the Kruskall-Wallis rank sum test

kruskal.test(weight ~ Diet, data = chicks_0_21)

# Load this for a non-parametric post-hoc test

library(pgirmess)

kruskalmc(weight ~ Diet, data = chicks_0_21)

