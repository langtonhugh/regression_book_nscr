# chapter_3_wim_012324.R
# (url: https://github.com/langtonhugh/regression_book_nscr/tree/main/scripts/chair)

# For tidy data processing
library(tidyverse)
# Access data from "Regression and other stories"
library(rosdata)

# THE EXERCISES

# Exercise 3.1 Weighted averages

# You often encounter weighted averages when you work with aggregated data, 
#   such as averages in subpopulations

# Groups: the categories that are being weighted (here four age groups)
# Shares: the proportions of each group in the sample
# Means:  the means values of 'something' in each group

# We first calculate total sample size by summing the four categories
n_sample = 200 + 250 + 300 + 250
n_sample
# [1] 1000

# Next we multiply the means of the groups with their share in the sample
50 * 200/n_sample + 
60 * 250/n_sample +
40 * 300/n_sample +
30 * 250/n_sample
# [1] 44.5

# We can also do this more systematically and in a tidy way

# Set up a little table that holds the relevant input data
tax_support <- 
  tibble(age_class        = c("18-29", "30-44", "45-64", "65+"),
         tax_support      = c(    50 ,    60 ,     40,   30),
         sample_frequency = c(   200 ,   250 ,    300,  250))
tax_support

# Note. I stumbled on the tribble function from the tibble package. This
#       allows inputting observations in rows and variables in columns.
#       Nice for small inline dataframes.

tax_support_alt <-
  tribble(
    ~age_class, ~tax_support, ~sample_frequency,
       "18-29",   50,   200,
       "30-44",   60,   250,
       "45-64",   40,   300,
       "65+"  ,   30,   250
)
tax_support_alt


tax_support |>
  # We first calculate the share in the sample of each group
  mutate(sample_share = sample_frequency / sum(sample_frequency),
  # Next multiply shares with percentage tax support       
         sample_tax_support = tax_support * sample_share) |>
  # And finally sum over the four groups
  summarize(sample_tax_support = sum(sample_tax_support))

# Exercise 3.2 Weighted averages

# Not sure whether I fully understood this exercise. I assume the idea is to
#   let us think about how other age distributions would affect the level
#   of support. Thus, if the tax support in the four age groups is given,
#   which age group distributions would yield an overall support percentage
#   of 40? 
#   An obvious but unrealistic distribution would consist of only
#   people aged 30-44, because among this group the support is precisely 40%
#
# Mathematically, the situation can be described with 2 equations with 4 unknowns
#
#   The first equation would just constrain the four weights to sum to 1
# (eq 1)  wght_18 + wght_30 + wght_45 + wght_65  = 1
#   
#   The second equation would constrain the weighted average to be 40
# (eq 2)  wght_18 * 50 + wght_30 * 60 + wght_45 * 40 + wght_65 * 30 = 40
#
# To find a deterministic solution, we need to assign values to two unknowns
#
# If we fix the shares of the least extreme age classes "18-29" and "45-64"
# to their original values (.2 and .3), we should be able to get an overall
# tax support of 40% by finding a suitable mix of age group 30-44 (support
# level 60%) and age group 65+ (support level 30%) .

# (eq 1) wght_30 + wght_65  = .5
# (eq 2) wght_30 * 60 + wght_65 * 30 = 40 - 22 = 18

# (eq 1) wght_30 = .5 - wght_65
# (eq 2) (.5 - wght_65) * 60 + weight_65 * 30 = 18

# (eq 1) wght_30 = .5 - wght_65
# (eq 2) 30 - wght_65 * 60 + weight_65 * 30 = 18

# (eq 1) wght_30 = .5 - wght_65
# (eq 2) 30 - 30 * wght_65 = 18

# (eq 1) wght_30 = .5 - wght_65
# (eq 2) 30 * wght_65 = 12

# (eq 1) wght_30 = .5 - 0.4 = .1
# (eq 2) wght_65 = 12/30 = .4


#So we get
  (50 * .2 + # this was fixed
   60 * .1 + # this was calculated
   40 * .3 + # this was fixed
   30 * .4)  # this was calculated
# [1] 40
# So it worked.

# SECTION 3.3 Plotting a line 

curve(expr = x,
      from = 0,
      to   = 20)
# Error in x(x) : could not find function "x"
# (I think the curve function needs at least one existing function name (log,
#  exp, sqrt, sin, cos, ...) or math symbol (+ - / * ^ ...) beyond the 'x'

curve(expr = x*1,
      from = 0,
      to   = 20)

# flat line
curve(expr = 1 + 0 * x,
      from = 0,
      to   = 20)

# flat line
curve(expr = 1 + 5 * x,
      from = 0,
      to   = 20)

curve(expr = log(1 + x),
      from = 0,
      to   = 20)

curve(expr = exp( x),
      from = 0,
      to   = 20)

curve(expr = sin(sqrt(x)),
      from = 0,
      to   = 2000)


# Mile record data

data("mile")
glimpse(mile)

# I guess that 'year' is a time variable that include monthnumber/12 as decimals
#  Lets us check the first two cases
1913 + 5/12
# [1] 1913.417
1915 + 7/12
# [1] 1915.583
# Correct

# I guess 'seconds' equals 60 * 'min' + 'sec'. Let's check
4 * 60 + 14.4
# [1] 254.4
4 * 60 + 12.6
# [1] 252.6
# Correct

# Base R
plot(mile$year, mile$seconds)

# ggplot (leave beauty contest stuff for later)
mile |>
  ggplot() +
  geom_point(aes(x = year, y = seconds))

# Estimate the line
fit <- lm(seconds ~ year, data = mile)

fit |> summary()

fitted_intercept <- fit$coeff["(Intercept)"]
fitted_slope <- fit$coeff["year"]

# ggplot (add the estimated regression line)
mile |>
  ggplot() +
  geom_point(aes(x = year, y = seconds)) +
  geom_abline(intercept = fitted_intercept, slope = fitted_slope)




# Exercise 3.3 Probability distributions

# Make sure we all get the same numbers
set.seed(123456789)

# Creating just 10 random numbers
standard_normal_10 <- rnorm(n = 10, mean = 0, sd = 1)
standard_normal_10

# Creating 1000 random numbers
standard_normal_1000 <- rnorm(n = 1000, mean = 0, sd = 1)
standard_normal_1000

# The most basic histogram (single variable distribution)
standard_normal_1000|>
  hist()


# Density for the standard normal (mean = 0, sd = 1)
x_axis <-
  seq(min(standard_normal_1000), 
      max(standard_normal_1000), 
      length = 40)

density <- dnorm(x_axis, 
                 mean = mean(standard_normal_1000), 
                 sd = sd(standard_normal_1000))

# By assumption (theoretical distibution has mean 0.00000 and sd 0.00000)
density_theoretical <- dnorm(x_axis, mean = 0, sd = 1)

plot(x = x_axis, y = density, type = "l")

# Creating 1000 random numbers
sd2_normal_1000 <- rnorm(n = 1000, mean = 0, sd = 2)
sd2_normal_1000

x_axis <-
  seq(min(sd2_normal_1000), 
      max(sd2_normal_1000), 
      length = 40)

density <- dnorm(x_axis, mean = mean(sd2_normal_1000), 
                 sd = sd(sd2_normal_1000))

plot(x = x_axis, y = density, type = "l") 



# Exercise 3.4

poisson_35_1000 <- 
  rpois(n = 1000, lambda = 3.5)

poisson_35_1000 |> hist(breaks = length(unique(poisson_35_1000)))

# Exercise 3.5 Binomial distribution

binom_03_20_1000 <- 
  rbinom(n = 1000, size = 20, prob = 0.3)

binom_03_20_1000 |> hist(breaks = length(unique(binom_03_20_1000)))

# Exercise 3.6 Linear transformations

# The mean must be increased from 35 to 100 by adding 65, 
# The standard deviation must be increased from 10 to 15 by multiplying with 1.5

# 
# Transformation:  X' = aX + b
# 
# mean(X') = a * mean(X) + b
# sd(X') = a * sd(X)
# 
# Now mean(X') = 100, mean(X) = 35, sd(X') = 15, sd(X) = 10
# 
# Substituting this:
#   100 = a * 35 + b
#   15 = a * 10
#   
#   100 = 15/10 * 35 + b
#   b = 47.5
#   a = (100 - b) / 35 = 1.5
#   
# Transformation: X` = 1.5 * X + 47.5  

original_scores = rnorm(n = 1000, mean = 35, sd = 10)
original_scores |> hist(breaks = 10)

transformed_scores = (original_scores * 1.5) + 47.5 
transformed_scores |> hist(breaks = 10)

# New range
# Lowest  (X=0) is 0 * 1.5 + 47.5 = 47.5
# Highest (X=50) is 50 * 1.5 + 47.5 = 122.5

# Simple
# First multiply to get the standard deviation right
transformed_1 <- original_scores * 1.5
# Check that is ~15 now
sd(transformed_1)
# What is the mean after the first transformation
mean(transformed_1)
# Add the difference between the target mean (100) and the current mean
transformed_2 <- transformed_1 + (100 - mean(transformed_1))
transformed_2 |> hist(breaks = 10)
  
plot(original_scores, transformed_scores, type = "l")

# Exercise 3.8 Correlated random variables

correlation_hw = .3
mean_husbands = 69.1
sd_husbands = 2.9
mean_wives = 63.7
sd_wives = 2.7

# weighted sum:
.5 * mean_husbands + .5 * mean_wives


# standard deviation of .5 * husband + .5 wife
sqrt(.5^2 * sd_husbands + 
       .5^2 * sd_wives + 
       2 * .5 * .5 * correlation_hw)








