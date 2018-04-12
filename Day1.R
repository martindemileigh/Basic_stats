#Day1.R
#This is a script about the Introduction to Statistics using R
#Purpose: To practice some of the concepts we will encounter
#Demi-Leigh
#12 April 2018.


# Load libraries ----------------------------------------------------------

library(tidyverse)

# Integers ----------------------------------------------------------------
# Nominal (discrete) data

# Generate some integer data
integer_r <- as.integer(seq(5, 14, by = 1))

# Running/Displaying the data
integer_r

# Look at a brief summary of them
summary(integer_r)

# Continuous Data ---------------------------------------------------------

#Generate a sequence of numeric values
numeric_r <- seq(23, 43, length.out = 10)

numeric_r

# Dates -------------------------------------------------------------------

# One may perform some arithmetic with dates
as.Date("2005-12-31") - as.Date("2005-12-12")

# Or for example
seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")

dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")

summary(dates_r)
# There is much more


# Dataframes --------------------------------------------------------------
# Adjust values so that they are the same
int [1:10]
num [1:10]
Date[1:10]

# Create the base dataframe

df_r <- data.frame(integers = integer_r,
                  numeric = numeric_r,
                  dates = dates_r)

# Then upgrade it to a tibble

df_r <- as_tibble(df_r)
summary(df_r)


# Categories --------------------------------------------------------------

# Electronics
elec_r <- as.factor(c("laptops",
                      "desktops",
                      "cellphones"))
# People
people_r <- as.factor(c("funnyhair",
                        "beautiful",
                        "beanies"))
# Colours
colour_r <- as.factor(c("red",
                        "blue"))
colour_r

summary(colour_r)


# Ordinal data ------------------------------------------------------------

# Here we still have qualitative data, but with some sort of order

colour_qual <- ordered(c("blue", "green",
                         "yellow", " orange", 
                         "red"),
                       levels = c("blue", "green",
                                   "yellow", " orange", "red"))

colour_qual



# Binary ------------------------------------------------------------------

# These are generally represented as TRUE or FALSE
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)


# Characters --------------------------------------------------------------

sites_r <- c("Yztervarkpunt", "Betty's Bay",
             "Gansbaai", "Sea Point")
sites_r



# Missing values ----------------------------------------------------------

# NO missing values
chicks_nest <- c(3, 2, 0, 10, 5)
summary(chicks_nest)

# Missing value
# Number of eggs recorded in a nest
# The NA shows a nest that was not able to be sampled
chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)

# A summary
summary(chicks_nest)

#The mean
mean(chicks_nest)

#The standard deviation
sd(chicks_nest)


# Viewing Data ------------------------------------------------------------

ChickWeight
summary(ChickWeight)
chicks <- ChickWeight
View(chicks)
head(chicks, 7)
tail(chicks, 15)
ChickWeight[1:3,]
ChickWeight[c(1,54,61,22),]




# Descriptive Statistics --------------------------------------------------

# First create a data frame
chicks <- as_tibble(ChickWeight)

# Count the Data
chicks %>% 
  summarise(chicken_count = n())
#OR
nrow(chicks)


# Measures of central tendency --------------------------------------------

# Calculate the Mean weight

chicks %>% 
  summarise(mean_wt = mean(weight))

# Be more specific
chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>%
  summarise(mean_wt = mean(weight))

# Can become less accurate with outliers
# What is another way for calculating the middle?? Median

chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>%
  summarise(mean_wt = mean(weight),
            median_wt = median(weight))

# Means and Medians of 1 and 3 are VERY different - Indication of Skewness

# visualise the density of the data
ggplot(data = filter (chicks, Time == 21), aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4)

# Skewness ----------------------------------------------------------------
# Imagine that you are squishing the data
# Calculate the numeric value
# First load libraries

library(e1071)

# Compare the difference in mean and median against skewness
# Mean behind the median = Skewed to the left
# Mean to the right or left of the median = Skewness

chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>%
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))

##Visualising the median and mean lines on the plots
# mean is the solid line


central_chicks <- chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>%
  summarise(mean_wt = mean(weight),
            median_wt = median(weight))


ggplot(data = central_chicks,
       aes(xintercept = mean_wt,
           colour = Diet), size = 1.5)
       filter (chicks, Time == 21), 
       aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4) 
  
 ##.....Incomplete code
  

# Kurtosis ----------------------------------------------------------------

# Calculate the kurtosis of the tails of a distribution
chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>%
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurtosis_wt = kurtosis(weight))

# Variance ----------------------------------------------------------------
# Measures of variability 
# Amount of variablility in the data or the actual variance

# Below is a summary of many different statistical properties
wt_summary <- chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight),
            wt_quart = quantile(weight, 0.25),
            wt_quart2 = quantile(weight, 0.5),
            wt_quart3 = quantile(weight, 0.75))

# Quartile = quarter values in the distribution
# Median = 50 percenth quartile (Middle quartile)
# Quantile = 
wt_summary  
  












