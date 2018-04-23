# Day5.R
# This is a script about the Introduction to Statistics using R
# Purpose: To revisit ANOVA tests using some examples and to learn about Regressions
# Demi-Leigh
# 20 April 2018.


# Load libraries ----------------------------------------------------------

library(tidyverse)
# library(Rmisc) # Unfortunately this overrides many dplyr functions
library(ggpubr)

# Load Data ---------------------------------------------------------------

snakes <- read_csv("snakes.csv")

snakes <- read_csv("snakes.csv") %>% 
  mutate(day = as.factor(day))

  # snakes$day = as.factor(snakes$day)
  # $ Makes available to act upon columns
  # <- Assign as.factor 
    # Mutate and the $ method achieve the same tthing!

# Manipulate the Data -----------------------------------------------------

snakes_summary <- snakes

snakes_summary <- snakes %>% 
  group_by(day) %>%
  summarise(snake_mean = mean(openings), 
            snakes_sd = sd(openings))

snakes_summary


# Formulate a hypothesis --------------------------------------------------

# HO: there is NO difference in the number of openings from day to day
# H1: There is a difference in the number of openings from day to day


# Test a hypothesis -------------------------------------------------------

# First calculate SE and CI
snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", 
                             groupvars = c("day"))

# then visualise the Data
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, 
               aes(x = day, xend = day, y = openings - ci, 
                   yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# BUT wait, we have two factors, so we need another null hypothesis
  # H0: There is no difference between snakes with respect to the number 
      # of openings at which they habituate.
  # H0: There is no difference between days in terms of the number of 
      # openings at which the snakes habituate.

# Fit ANOVA to hypothesis -------------------------------------------------

# Test just for the days hypothesis
snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)

# Report in results sections:
  # Need df - fed bck to people reading
  # Sum of squares or mean sq
  # F - table
  # Probablity less than 0.05
  # Sum of square - amount of variance

# Test both hypothesis
snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)


# Testing assumptions afterwards ------------------------------------------

# First test normality of the data
snakes.residuals <- residuals(snakes.all.aov)
hist(snakes.residuals)

# Then visualise homoscedasticity of results
plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

# Check Tukey results
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)

# Visualise the factor interaction
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 1) +
  geom_point(size = 4)

# Exercise ----------------------------------------------------------------

# Factor 1 = Location
# Factor 2 = Traps
# Significance of location * trap

# Load Data

read_csv("moth_traps.csv")

moths <- read_csv("moth_traps.csv") %>% 
  gather(key = "trap", value = "count", -Location)

# Formulate hypothesis ----------------------------------------------------

# H0: There is no difference between the count in relations to the Location
    # of the trap on the tree.
# H0: 

# Test both hypothesis
moths.aov <- aov(count ~ trap * Location, data = moths)
summary(moths.aov)

# First test normality of the data
moths.residuals <- residuals(moths.aov)
hist(moths.residuals)

# Then visualise homoscedasticity of results
plot(fitted(moths.aov), residuals(moths.aov))

# Check Tukey results
moths.tukey <- TukeyHSD(moths.aov, which = "Location")
plot(moths.tukey)

# Visualise the factor interaction

moth.summary <- summarySE(data = moths, measurevar = "count",
                           groupvars = c("trap"))

ggplot(data = moths, aes(x = trap, y = count)) +
  geom_segment(data = moth.summary,
               aes(x = trap, xend = trap, y = count - ci, 
                   yend = count + ci, colour = trap),
       size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) +
  geom_jitter(width = 0.05)


plt1 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot()

plt2 <- ggplot(data = moths, aes(x = trap, y = count, fill = trap)) +
  geom_boxplot()

plt3 <- ggplot(data = moths, aes(x = Location, y = count, fill = trap)) +
  geom_boxplot()


ggarrange(plt1, plt2, plt3, nrow = 3, labels = "AUTO")


# Regressions -------------------------------------------------------------


# For the explanation of this statistical analysis
# we are going to use eription data from Ol' Faithful

head(faithful)

# Plot a quick scatterplot

ggplot(data = faithful, aes( x = waiting, y = eruptions))+
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "hotpink")

# If we were to adjust the scales to start at 0
ggplot(data = faithful, aes( x = waiting, y = eruptions))+
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "hotpink") +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(-2, 6))

# There is  a linear relationship betwen waiting time and the eruptions
# N0: Waiting time does not influence the duration of an eruption
# N1: Waiting time does influence the duration of an eruption

# Test a hypothesis -------------------------------------------------------

faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm)
 #To test the hypothesis (Intercept)

slope <- round(faithful_lm$coef[2], 3)

r2 <- round(summary(faithful_lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", 
                                                 slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  #annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val)
  #         , parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), 
           parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")


# Correlations ------------------------------------------------------------

# Load library ------------------------------------------------------------
library(corrplot)

# Load Data
ecklonia <- read_csv("ecklonia.csv")


# Formulate a hypothesis --------------------------------------------------

# H0: There is no relationship between stipe length and stipe diameter
    # for the kelp Ecklonia maxima
# H1: There is relationship between stipe length and stipe diameter
    # for the kelp Ecklonia maxima


# Test a hypothesis -------------------------------------------------------

cor.test(x = ecklonia$stipe_length, 
         ecklonia$stipe_diameter) 
       
# Visualise
ggplot(data = ecklonia, aes(x = stipe_length, y = stipe_diameter)) +
         geom_point()

# Run a hecka tests at once

ecklonia_sub <- ecklonia %>% 
  select(stipe_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor


# Spearman rank test ------------------------------------------------------

# First create a ordinal column
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), 3))

# Then run a Spearman test
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")


# Kendall rank correlation ------------------------------------------------
cor.test(ecklonia$primary_blade_length, 
         ecklonia$primary_blade_width,
         method = "kendall")

# visualise all things 

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

corrplot(ecklonia_pearson, method = "circle")

# Heat Map Exercise

library(reshape2)
melted_ecklonia <- melt(ecklonia_pearson)

ggplot(data = melted_ecklonia, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value), colour = "white")

