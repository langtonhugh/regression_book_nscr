################################################################################
# Title: Regression and Other Stories Reading Club Session (chapter 1) 
#        NSC-R workshop series. 
# Date: 05.12.2023
# Script author: Sam Langton (NSCR)
################################################################################

# Load packages.
library(dplyr)
library(ggplot2)

# Adjusting for pre-treatment differences (page 11).

# Create data.
set.seed(1612)

N <- 100
pretreat_x <- rnorm(N, 0, 1)^2
treat_x <- rep(0:1, N/2)
pretreat_x <- ifelse(treat_x==0, rnorm(N, 0, 1.2)^2, rnorm(N, 0, .8)^2)
y <- rnorm(N, 20 + 5*pretreat_x + 10*treat_x, 3)
sim_data <- data.frame(pretreat_x, treat_x, y) %>% 
  as_tibble() %>% 
  mutate(treat_x = as.factor(treat_x))

# Basic mean difference between treatment-control groups in Y.
sim_data %>% 
  group_by(treat_x) %>% 
  summarize(mean_values = mean(y))

# Plot of treatment-control differences in the Y.
# We add a Mean Standard error from bootstrap.
ggplot(data = sim_data,
       mapping = aes(x = treat_x, y = y)) +
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "red")

# Simple test of treatment-control differences in the Y.
lm_1 <- lm(y ~ treat_x, data=sim_data)

# Results.
summary(lm_1) 

# What about pre-treatment differences in the treatment-control groups?
sim_data %>% 
  group_by(treat_x) %>% 
  summarise(mean_value = mean(pretreat_x))

# Is there a relationship between pre-treatment and Y?
ggplot(data = sim_data,
       mapping = aes(x = treat_x, y = pretreat_x)) +
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "red")

ggplot(data = sim_data,
       mapping = aes(x = pretreat_x, y = y, group = treat_x, colour = treat_x)) +
  geom_point(alpha = 0.5)

# Run linear model predicting Y using the treatment X while controlling for
# the pre-treatment X.
lm_2 <- lm(y ~ pretreat_x + treat_x, data=sim_data)


# Plotting potential pre-treatment differences in the Y. 
ggplot(data = sim_data,
       mapping = aes(x = pretreat_x, y = treat_x)) +
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot", colour = "red")

# Assessing treatment-control differences in the Y while grouping
# for the pre-treatment differences.
ggplot(data = sim_data) +
  geom_point(mapping = aes(x = pretreat_x, y = y, colour = treat_x)) +
  geom_smooth(mapping = aes(x = pretreat_x, y = y, colour = treat_x), method = "lm", se = FALSE) 

# Run the regression.




