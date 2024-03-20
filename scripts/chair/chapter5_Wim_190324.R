# ROS chapter 5: simulation

library(tidyverse)

# Simulation is all about random generating random numbers.
# Let's first wrap up some basics of random number generation

# Random numbers ---------------------------------------------------------------

# Random sampling from a uniform distribution
# (within a given range, every number is equally likely)

# A single random number between 0 and 1 (repeated to show that each
#  consecutive number is different from the previous numbers)
runif(n = 1, min = 0, max = 1)
runif(n = 1, min = 0, max = 1)
runif(n = 1, min = 0, max = 1)
runif(n = 1, min = 0, max = 1)

# For reproducibility we need to set the seed ('starting value')
set.seed(12345)
runif(n = 1, min = 0, max = 1)
runif(n = 1, min = 0, max = 1)
runif(n = 1, min = 0, max = 1)
# To repeat the previous sequence of three, reset the seed
set.seed(12345)
runif(n = 1, min = 0, max = 1)
runif(n = 1, min = 0, max = 1)
runif(n = 1, min = 0, max = 1)

# We can use other ranges than 0-1
runif(n = 1, min = 0, max = 100)
runif(n = 1, min = 10, max = 20)
runif(n = 1, min = -2, max = 2)

# We can generate any quantity of random numbers, e.g. 12 random numbers
#   between 0 and 100
set.seed(12345)
runif(n = 12, min = 0, max = 100)
# If we wanted just whole numbers, we can truncate them
set.seed(12345)
runif(n = 12, min = 0, max = 100) |> 
  trunc()

# The range can be very large if needed
runif(n = 12, min = -10000000, max = 10000000)

# Binomial distribution: How many successes in [size] trials where each
#    trial has a [prob] probability of success?

rbinom(n = 1, size = 10, prob = .50)
rbinom(n = 1, size = 10, prob = .50)
rbinom(n = 1, size = 10, prob = .50)
rbinom(n = 1, size = 100, prob = .50)
rbinom(n = 1, size = 100, prob = .50)
rbinom(n = 1, size = 100, prob = .50)

# Now we can also repeat running 10 trials and counting successes, e.g.
#   5 times count the number of successes in 10 trials
rbinom(n = 5, size = 10, prob = .50)
#   50 times count the number of successes in 100 trials 
rbinom(n = 50, size = 100, prob = .50)
# Run 10000 times and plot (I prefer the hist() function for quick histograms)
rbinom(n = 10000, size = 100, prob = .50) |> 
  hist()


# Our 'best friend' is the normal distribution
rnorm(n=1, mean = 100, sd = 15)
rnorm(n=1, mean = 100, sd = 15)

# And just like rbinom we can easily draw numbers many times 
rnorm(n=1000, mean = 100, sd = 15) |> 
  hist()

# Here is the Poisson distribution
rpois(n = 10, lambda = 3)
rpois(n = 10, lambda = 8)

# negative binomial distribution
rnbinom(n = 1,  size = 1, mu = 5)
rnbinom(n = 10, size = 1, mu = 6)

# I myself once used the zero-truncated Poisson distribution
#install.packages("actuar")
library(actuar)     # rztpois (random zero-truncated poisson)
rztpois(10, 3)

# Frequency table of result
rztpois(10000, 3) |> 
  as_tibble() |>
  count(value)

# Why no white space between 0 and 1?
rztpois(10000, 3) |> 
  hist()

rztpois(10000, 5) |> 
  as_tibble() |>
  ggplot() +
  geom_histogram(aes(value))

# Writing functions for simulation ---------------------------------------------

# Code from ROS p. 69
n_girls <- rbinom(1, 400, 0.488)
print(n_girls)

# Running the simulation in a self-programmed loop
n_sims <- 1000
n_girls <- rep(NA, n_sims)
for(s in 1:n_sims) {
  n_girls[s] <- rbinom(1, 400, 0.488)
}
head(n_girls)

# Using the 'canned solution' is much easier
#  (first argument of rbinom is # of replications)
n_girls <- rbinom(1000, 400, 0.488)
head(n_girls)

# Check whether these two are truly equivalent by setting the seed (they are)
set.seed(12345)
n_sims <- 1000
n_girls <- rep(NA, n_sims)
for(s in 1:n_sims) {
  n_girls[s] <- rbinom(1, 400, 0.488)
}
head(n_girls)

set.seed(12345)
n_girls <- rbinom(1000, 400, 0.488)
head(n_girls)


# Writing a custom function (as on p.72)
count_girls <- function(births, prob) {
  rbinom(1, births, prob)
}
#  Use the replicate function to call this function 10 times
set.seed(12345)
replicate(10, count_girls(births = 400, prob = 0.488))
# Note that by setting the seed I get the same output is before

# As an exercise, let's try write our own rbinom() function:
#   It uses the same arguments as rbinom
my_rbinom <- function(n, size, prob) {
  # Create a vector of length n and fill with NA 
  n_sims <- rep(NA, n)
  # for each trial
  for(i in 1:n) {
    # generate n random numbers between 0 and 1
    random_number <- runif(n = size, min = 0,  max = 1 )
    # store '1' if < prob (i.e. success) and '0' otherwise
    n_sims[i] <- sum(random_number < prob)
  }
  n_sims
}
my_rbinom(10000, 100, .50) |> hist()
rbinom(10000, 100, .50) |> hist()

# What about seed-setting in this case?
set.seed(12345)
rbinom(10, 50, .10) 
set.seed(12345)
my_rbinom(10, 50, .10) 


# Trying to understand how replicate function works
# ?replicate  (similar to apply: repeat a function 1 or more times)

# Just replicate the number 34 two times
rep_result1 <- replicate(n = 2, expr = 34) |>
  print()

# lets use a more interesting expression (function): random draw from normal
#    distribution (set.seed for checking both methods return the same vector)
set.seed(12345)
rep_result2 <- replicate(n = 2, expr = rnorm(n=1, mean=0, sd=1), simplify = "array" ) |>
  print()
# Of course, this could also be done like this
set.seed(12345)
rnorm(n=2, mean=0, sd=1)

# Is the result a numeric vector?
class(rep_result2)
typeof(rep_result2)

# What about a function that itself returns a vector (of 3 numbers, in this case)
rep_result3 <- replicate(n = 2, expr = rnorm(n=3, mean=0, sd=1)) |>
  print()
# Let's see what the result is
class(rep_result3)
typeof(rep_result3)
# So if the expression we give to replicate returns a vector, replicate returns 
#   a matrix in which the columns represent the replications and the columns
#   the vector elements

# Let's write a function that produces a random standard normal distribution of 
#  n=100 and return the mean and standard deviation and median absolute devision
#  in a vector

make_normal <- function() {
  distribution <- rnorm(100, mean = 0, sd = 1)
  mean <- mean(distribution)
  sd <- sd(distribution)
  mad <- mad(distribution)
  c(mean, sd, mad)
}

# Let's run this function a single time
function_result1 <- make_normal() |>
  print()
class(function_result1)
typeof(function_result1)

# Run it a few times
make_normal() 
make_normal() 
make_normal() 
make_normal() 

# replicate it
replicate(5, make_normal)

# OOPS, what is this? 
# replicate accepts any valid expression, so when I type 'make_normal' 
#   without parentheses (brackets) is does not interpret it as 
#   a function but as a function definition, repeating it 5 times
#   and storing it in a list

# I should have typed:
replicate(5, make_normal())



# EXERCISE 5.1
# Discrete probability simulation: Suppose that a basketball player has a 60% chance of making
# a shot, and he keeps taking shots until he misses two in a row. Also assume his shots are
# independent (so that each shot has 60% probability of success, no matter what happened before).

# (a) Write an R function to simulate this process.
# Assume: The function should return the numbers of shots taken, including the
#         two last ones that were missed
# The function takes one argument: the probability of making a shot
n_shots <- function(prob) {
  # Set the shot counter to zero
  shots <- 0
  # The function should remember whether the last shot failed 
  failed = 0
  while (failed < 2) {
    shots <- shots + 1
    if (runif(1, min = 0, max = 1) < prob) {
      failed <-0
    }  # end if if
    else {
      failed <- failed + 1
    } # end of else
  } # end of while
  shots
} # end of function

n_shots(.60)

# (b) Put the R function in a loop to simulate the process 1000 times. Use the simulation to estimate
# the mean and standard deviation of the total number of shots that the player will take, and plot
# a histogram representing the distribution of this random variable.

series_1000 <- replicate(1000, n_shots(.60))
series_1000 |> mean()
series_1000 |> sd()

# (c) Using your simulations, make a scatterplot of the number of shots the
#     player will take and the proportion of shots that are successes.

shots_rate <-
  tibble(shots = series_1000) |>
  mutate(percentage_success = 100 * (shots - 2) / shots)

shots_rate_ggp <-
  shots_rate |>
  ggplot() +
  geom_point(aes(x = shots, y = percentage_success))
  
shots_rate_ggp


# EXERCISE 5.2 Continuous probability simulation: 
# The logarithms of weights (in pounds) of men in the United
# States are approximately normally distributed with mean 5.13 and standard deviation 0.17;
# women’s log weights are approximately normally distributed with mean 4.96 and standard
# deviation 0.20. Suppose 10 adults selected at random step on an elevator with a capacity of 1750
# pounds. What is the probability that their total weight exceeds this limit?
#   
# Let's first define sampling weights for men

# In log(pounds) for men
rnorm(1, 5.13, 0.17)
# In pounds
exp(rnorm(1, 5.13, 0.17)) 
# In kilos (1 pound = .4535 kilos)
exp(rnorm(1, 5.13, 0.17)) *.4535

# In log(pounds) for women
rnorm(1, 4.96, 0.20)
# In pounds
exp(rnorm(1, 4.96, 0.20)) 
# In kilos
exp(rnorm(1, 4.96, 0.20)) *.4535

n_adults <- 10
weights <- rep(NA, n_adults)
for(a in 1:n_adults) {
  # pick random gender (male or female)
  if(runif(1, 0, 1) > .50) {
    # if male: pick random weight from male normal distribution
    logweight <-rnorm(1, 5.13, 0.17)  
  } else {
    # if female: pick random weight from female normal distribution
    logweight <- rnorm(1, 4.96, 0.20)
  } 
  weights[a] <- exp(logweight)
}
 # Some to see how much the 10 random people weight together
sum(weights)

# Make this into a function

sum_weights <- function(n) {
  n_adults <- n
  weights <- rep(NA, n_adults)
  for(a in 1:n_adults) {
    if(runif(1, 0, 1) > .50) {
      logweight <-rnorm(1, 5.13, 0.17)  
    } else {
      logweight <- rnorm(1, 4.96, 0.20)
    } 
    weights[a] <- exp(logweight)
  }
  # Return 1 if total weight exceeds 1750 pounds
  sum(weights) > 1750  
}

# What is probability of elevator overweight?
mean(replicate(10000, sum_weights(10)))




# EXERCISE 5.3 Binomial distribution: 
# A player takes 10 basketball shots, with a 40% probability of making
# each shot. Assume the outcomes of the shots are independent.

# (a) Write a line of R code to compute the probability that the player makes exactly 3 of the 10
# shots.

# Highschool n = trials, s = successes p = probability of success
# P(x=n) = p^s * (p-1)^(n-s) * (n! / ((n-s)! * s!)
#          -----------------   -------------------
#           .4^3 * .6^7 * (10! / ((10-3)! * 3!))
# where 10! = 10 * 9 * 8 * 7 * ... * 2 * 1

# Let's first write a function to calculate a factorial

# Iterative
fact_iter <- function(x) {
    result <- 1
    for(i in 1:x) {
      result = result *  i
    }
    result
}
fact_iter(1)
fact_iter(2)
fact_iter(3)
fact_iter(4)

# Recursive
fact_recurse <- function (x) {
  if (x == 1) {
    return(1)
  } else {
      return(x * fact_recurse(x-1))
  }
}

fact_recurse(1)
fact_recurse(2)
fact_recurse(3)
fact_recurse(4)

# But R has a 'factorial' function too
factorial(3)
factorial(4)

# Back to exercise 5.3:
# Highschool n = trials, s = successes p = probability of success
# P(x=n) = p^s * (p-1)^(n-s) * (n! / ((n-s)! * s!)
#          -----------------   -------------------
#           .4^3 * .6^7 * (10! / ((10-3)! * 3!))
# where 10! = 10 * 9 * 8 * 7 * ... * 2 * 1

prob3_10 <- .4^3 * .6^7 * factorial(10) / (factorial(7) * factorial(3))
prob3_10

# The dbinom() function returns the probability of x successes in n trials
#    with probability p
# For example, the probability of 1 success in 1 trial with probability .6
dbinom(1,1,0.6)
# [1] 0.6
# And the probability of exactly 1 success in 2 trials with probability .5
dbinom(1,2,0.5)
# [1] 0.5
# And the probability of exactly 2 successes in 2 trials with probability .5
dbinom(2,2,0.5)
# So the probability of exactly 3 successes in 10 trials with probability .40
dbinom(3, 10, 0.40)
# [1] 0.2149908
prob3_10
# 1] 0.2149908

# Highschool math: .40^3 * .60^7 * (10!) / ((10-3)! * 3!) 

# (b) Write an R function to simulate the 10 shots. Loop this function 10 000 times and check
# that your simulated probability of making exactly 3 shots is close to the exact probability
# computed in (a).
x_out_of_y_shots <- function(successes, shots, prob) {
  as.numeric(sum(runif(shots,min = 0, max = 1) < prob) == successes)
}

# Or in steps (explicit and better readable)
x_out_of_y_shots <- function(successes, shots, prob) {
  # take a random number from the uniform distribution on interval <0,1>
  p <- runif(shots,min = 0, max = 1) 
  # is it a success
  q <- p < prob
  # convert FALSE/TRUE to 0/1
  r <- as.numeric(q)
  # sum the successes
  s <- sum(r)
  # Is the sum exactly what we wanted to count
  t <- s == successes
  # convert FALSE/TRUE to 0/1
  u <- as.numeric(t)
  u
}  
  

x00 <- replicate(10000, x_out_of_y_shots(0, 10, .40)) |> mean()
x01 <- replicate(10000, x_out_of_y_shots(1, 10, .40)) |> mean()
x02 <- replicate(10000, x_out_of_y_shots(2, 10, .40)) |> mean()
x03 <- replicate(10000, x_out_of_y_shots(3, 10, .40)) |> mean()



# EXERCISE 5.5 
# Distribution of averages and differences: The heights of men in the United States are approximately
# normally distributed with mean 69.1 inches and standard deviation 2.9 inches. The heights of
# women are approximately normally distributed with mean 63.7 inches and standard deviation 2.7
# inches. Let x be the average height of 100 randomly sampled men, and y be the average height of
# 100 randomly sampled women. In R, create 1000 simulations of x - y and plot their histogram.
# Using the simulations, compute the mean and standard deviation of the distribution of x - y and
# compare to their exact values.

# Exact values:
# MEAN: mean(x-y) = mean(x) - mean(y)
# SD:   sd(x-y) = sqrt(sd(x)^2 + sd(y)^2)
# MEAN: 
69.1 - 63.7
# SD
sqrt(2.9^2 + 2.7^2)

# Simulations
men_heights <- rnorm(1000, mean = 69.1, sd = 2.9)
women_heights <- rnorm(1000, mean = 63.7, sd = 2.7)
height_differences  <- men_heights - women_heights
# Plot histogram of differecnes
height_differences |> hist()
# mean and sd
height_differences |> mean()
height_differences |> sd()


compute_difference <- function(n) {
  men_heights <- rnorm(100, mean = 69.1, sd = 2.9)
  women_heights <- rnorm(100, mean = 63.7, sd = 2.7)
  height_differences  <- men_heights - women_heights
  height_differences
}  

# Increase sample sizes
height_differences <- compute_differences(100)
height_differences |> mean()
height_differences |> sd() 

height_differences <- compute_differences(1000)
height_differences |> mean()
height_differences |> sd() 

height_differences <- compute_differences(100000)
height_differences |> mean()
height_differences |> sd() 

height_differences <- compute_differences(100000000)
height_differences |> mean()
height_differences |> sd() 


sample_differences <- 
  compute_difference(100) |>
  as_tibble() |>
  rename(difference = value)

sample_differences |>
  ggplot() +
  geom_histogram(aes(difference), binwidth = 2)

sample_differences |> mean()

replicate(1000, compute_differences(100)) |> class()

# ROS p. 74

library(rosdata)

earn <- earnings$earn
male <- earnings$male
ratio <- median(earn[male == 0]) / median(earn[male == 1])

# Let try to do this the tidy way

earnings |>
  group_by(male) |>
  summarize(median_earnings = median(earn), .groups = "drop") |>
  pivot_wider(names_from = male, values_from = median_earnings) |>
  mutate(ratio =  `0`/ `1`) |>
  pull()
  
# Function like ROS p. 74

boot_ratio <- function(data) {

    n <- nrow(data)
  
    booted_data <- 
      data |>
      slice_sample(n =n , replace = TRUE)
    
    booted_data |>
    group_by(male) |>
    summarize(median_earnings = median(earn), .groups = "drop") |>
    pivot_wider(names_from = male, values_from = median_earnings) |>
    mutate(ratio =  `0`/ `1`) |>
    pull()

}

boot_ratio(earnings)
