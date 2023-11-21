################################################################################
# Title: Regression and Other Stories Reading Club Session (chapter 1) 
#        NSC-R workshop series. 
# Date: 14.11.2023
# Script author: Alex Trinidad
# Affiliation 1: University of Cologne
# Affiliation 2: Netherlands Institute for the Study of Crime and Law Enforcement
################################################################################
library(tidyverse)

# 1. Regression to predict ------------------------------------------------
# Predict presidential vote share using economy growth
# Load the ROS data

elections_data <- read.csv(url("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat"), sep = "")

# Also
# remotes::install_github("avehtari/ROS-Examples", subdir = "rpackage")
# elections_data <- rosdata::hibbs

# Explore economy growth

glimpse(elections_data)

View(elections_data)

ggplot(data = elections_data) +
  geom_point(aes(x = year, y = growth))

ggplot(data = elections_data) +
  geom_point(aes(x = year, y = growth)) +
  geom_smooth(aes(x = year, y = growth), se = FALSE)

ggplot(data = elections_data) +
  geom_point(aes(x = year, y = growth)) +
  geom_smooth(aes(x = year, y = growth), se = TRUE)

# fit ols regression to obtain the predicted values

mod1 <- lm(vote ~ growth, data = elections_data)

summary(mod1)

# Plot predicted values

plot(elections_data$growth, elections_data$vote, xlab = "Economic Growth", ylab = "Vote Share")
abline(coef(mod1), col = "red")

ggplot(data = elections_data) +
  geom_point(aes(x = growth, y = vote)) +
  geom_abline(intercept = mod1[[1]][[1]], slope = mod1[[1]][[2]], color = "red", size = 1) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) + 
  geom_hline(yintercept = 50) +
  labs(title = "Data and linear fit",
       x = "Average recent growth in personal income",
       y = "Incumbent party's vote share")


ggplot(data = elections_data) +
  geom_point(aes(x = growth, y = vote)) +
  geom_smooth(method = "lm", aes(x = growth, y = vote), color = "blue", size = 1) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) + 
  geom_hline(yintercept = 50) +
  labs(title = "Data and linear fit",
       x = "Average recent growth in personal income",
       y = "Incumbent party's vote share")


# 2. Sketching regression -------------------------------------------------

# Original y = 46.3 + 3.0 x 
# Explore the descriptive stats to get some parameters based on the observed data
elections_data |> 
  summarise(min_growth = min(growth),
            max_growth = max(growth),
            mean_growth = mean(growth),
            sd_growth = sd(growth),
            min_vote = min(vote),
            max_vote = max(vote),
            mean_vote = mean(vote),
            sd_vote = sd(vote))

# Simulating the data
set.seed(123)
N <- 16
simu_growth <- runif(N, -0.39, 4)
simu_vote <- rnorm(N, 46.2476  + 3.0605*simu_growth, 3.763)
simu_elections <- data.frame(N,simu_growth, simu_vote)

simu_mod <- lm(simu_vote ~ simu_growth, data = simu_elections)

summary(simu_mod)

# Base graphic
plot(simu_elections$simu_growth, simu_elections$simu_vote, xlab = "Simulated Economic Growth", ylab = "Simulated Vote Share")
abline(coef(simu_mod), col = "blue")

# ggplot version 

ggplot(data = simu_elections) +
  geom_point(aes(x = simu_growth, y = simu_vote)) +
  geom_smooth(method = "lm", aes(x = simu_growth, y = simu_vote), color = "blue", size = 1) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) + 
  geom_hline(yintercept = 50) +
  labs(title = "Simulated Data and linear fit",
       x = "Simulated Average recent growth in personal income",
       y = "Simulated Incumbent party's vote share")

# Exercise 1.2 from ROS
# a) y = 30 + 10x  (residual sd 3.9)
# values of X ranging from 0-4 

set.seed(123)
N <- 50
x <- runif(N, 0, 4)
y <- rnorm(N, 30 + 10*x, 3.9)
data <- data.frame(N, x, y)

lm_a <- lm(y ~ x, data)

plot(data$x, data$y, xlab = "X Value", ylab = "Y value")
abline(coef(lm_a), col = "red", size = 1)


# b) y = 30 + 10x  (residual sd 10)
# values of X ranging from 0-4 

set.seed(123)
N <- 50
x <- runif(N, 0, 4)
y <- rnorm(N, 30 + 10*x, 10)
data <- data.frame(N, x, y)

lm_b <- lm(y ~ x, data)

plot(data$x, data$y, xlab = "X Value", ylab = "Y value")
abline(coef(lm_b), col = "blue")

# Now simulate a binary predictor (example from the Aki Vehtari GH: https://avehtari.github.io/ROS-Examples/SimpleCausal/causal.html)

# Figure 1.5
set.seed(1411)
N <- 50
x <- runif(N, 0, 4)
y <- rnorm(N, 30 + 10*x, 10)
x_binary <- ifelse(x < 3, 0, 1)
data_simu <- data.frame(N, x, y, x_binary)

lm_binary <- lm(y ~ x_binary, data = data_simu)

summary(lm_binary)

# Plot the relationship 
ggplot(data = data_simu) +
  geom_point(aes(x = x_binary, y = y)) +
  geom_abline(intercept = lm_binary[[1]][[1]], slope = lm_binary[[1]][[2]],
              color = "blue", size = 1) +
  labs(y = "Crime reduction", 
       x =  NULL) +
  scale_x_continuous(breaks = c(0,1),
                     labels = c("Control", "Treatment")) +
  annotate(geom = "text", x = 0.50, y = 40,
           label = paste("Estimated treatment effect is\nslope of fitted line: ",
                         round(lm_binary[[1]][[2]], digits = 2)))

# Non-linear relationship 
set.seed(1411)
x <- runif(N, 1, 7)
y <- rnorm(N, 7 + 30*exp(-x), 2)
data_simu$y <- y

# Fit the model 
lm_nonlinear <- lm(y ~ x, data = data_simu)

summary(lm_nonlinear)
           
# Plot the model outcome
ggplot(data = data_simu) +
  geom_point(aes(x = x, y = y)) +
  geom_smooth(method = "loess", aes(x = x, y = y), color = "blue", size = 1, se = FALSE) +
  labs(y = "Theft counts per hour", 
       x =  "Hours of foot patrol")  
