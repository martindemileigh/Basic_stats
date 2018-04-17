# Day2.R
# This script is a continuation of the Introduction to Statistics using R
# The day in which we discuss data visualisations and distributions
# Demi-Leigh
# 13 April 2018


# Load libraries ----------------------------------------------------------

library(tidyverse)


# Manual calculations -----------------------------------------------------

# Generating some random normal data
# rnorm generates random normal data [Arguments:  n = number of samples, mean, sd)

r_dat <- data.frame(dat = rnorm(n = 600,mean = 372,sd = 50),
                    sample = "A")

# Quick visualisation
# Histograms or Density plot

ggplot(data = r_dat, aes(x = dat)) +
  geom_density()

# The Mean
# The sum of all the points 
  # divided by 
  # the number of all the points

r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_fun = mean(dat))

# The median 
# Subset data (n+1)/2
# Brute force with base R
# $ tells R that you have a data frame on the left HS
# Order data from smallest to data and then you take the middle value = Different values

order(r_dat$dat)[(length(r_dat$dat)+1)/2]

# Or use tidy
# slice does not slice a column = provides entire row

r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)

# Or the tidy automagic way

r_dat %>% 
  summarise(r_median = median(dat))

# Variance
# The sum of 
  # each value 
    # minus 
      # the mean
         # Squared
# Divided by 
  #the count of samples minus one

r_dat %>% 
  mutate(r_error = dat-mean(dat),
         r_error_square = r_error * r_error) %>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n()-1),
           # OR use the built in function
             r_var_func = var(dat))
  
# The Standard deviation

r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))
 

# Exercise 1 ---------------------------------------------------------
  # (End of Chaper 3)
  # Notice how the data summary for chicken weights contained within 
  # wt_summary is very similar to the summary returned for weight when we apply summary(chicks). Please use the summarise() approach and construct a data summary with exactly the same summary statistics for weight as that which summary() returns.

# What does summary return when using the ChickWeight data?

summary(ChickWeight$weight)

# How to reproduce this in tidy?

ChickWeight %>% 
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mean_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))



# Visualisations ----------------------------------------------------------

# First Load Libraries
# These few packages contain most functions necessary
# to make publication ready figures

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)

# Load our SA time data

sa_time <- read_csv("SA_time.csv")

sa_time

# Edit our data

sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1)) # seq(starting from row n, n(), Up to nrows)

sa_time 

# Create long data

sa_long <- sa_time %>% 
  gather(key = "time_type",value = "minutes", -human)

sa_long

# Qualitative -------------------------------------------------------------
  # Showing count or the proportion of it against other things 

# Create a count of qualitative values
sa_count <- sa_long %>%
  count(time_type) %>% 
  mutate(prop = n / sum(n)) 

sa_count

# Stacked bar graphs

ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +     # geom() = error; Needs stat = "identity"
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal() # Allows us to produces figures that are publication ready

# Stacked proportion bar graph

ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

# Pie chart...

ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +    
  labs(title = "Pie chart", subtitle = "but why though?",
       x = NULL, y = "NULL") +
  coord_polar("y", start = 0) +
  theme_minimal()

# Continuous Data ------------------------------------------------------------

#  Histograms

ggplot(data = sa_long, aes( x = minutes)) +
  geom_histogram()
  
# Oh no!
# Let's get rid of that one value...

sa_clean <- sa_long %>% 
  filter(minutes < 100)

# A faceted histogram
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type, position = "dodge")) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")


# Relative proportion histogram

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(y = ..density.. ,fill = time_type, 
                     position = "dodge", binwidth = 1)) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Boxplots
# Interquartile range - Measure of central tendency - Spread of the data

ggplot(dat = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))

# Notched boxplots

ggplot(dat = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)

?geom_boxplot

# Calculate summary stats for plotting over the boxplots

sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

# Plot these means over boxplots

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
  aes(y = time_type_mean), colour = "goldenrod")

# Relationships -----------------------------------------------------------

# A basic scatterplot

ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))


# Editing the data to add locations

sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1), 
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
                 rep("Joburg", 2))) 

# Adding trend lines

ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))

