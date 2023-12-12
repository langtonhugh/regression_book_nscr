################################################################################
# Title: Regression and Other Stories Reading Club Session (chapter 2) 
#        NSC-R workshop series. 
# Date: 12.12.2023
# Script author: Sam Langton (NSCR)
################################################################################

# Load packages.
library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(broom)
library(maps)
library(sf)

# (0) General discussion points ================================================
# - New GitHub repo intro.
# - Difference between visualisation issues and then scrutiny over a composite
#   measure (section 2.1).
# - Validity and reliability: any examples from people's work?
# - What did people think of the plots? I had some queries (e.g., 2.5, 2.7).
# - Grammar of Graphics-style approach (but without ggplot2 code).
# - Plotting regression results.

# (1) Map visual ===============================================================

# Load in HDI data.
hdi_df <- read.table("data/ROS-Examples-master/HDI/data/hdi.dat", header = TRUE)

# Get state boundaries using the {maps} package.
states_sf <- maps::map("state",
                       plot = FALSE,
                       fill = TRUE) %>% 
  st_as_sf()

# Plot.
ggplot(data = states_sf) +
  geom_sf()

# Join the HDI data to the spatial polygons.
state_hdi_sf <- hdi_df %>% 
  mutate(ID = str_to_lower(state)) %>% 
  right_join(states_sf) %>% 
  st_as_sf()

# Map out HDI using a continuous scale.
ggplot(data = state_hdi_sf) +
  geom_sf(mapping = aes(fill = hdi)) +
  scale_fill_viridis_c()

# (2) Scatterplots =============================================================

# Load in the income/hdi data.
income_df <- read.dta("data/ROS-Examples-master/HDI/data/state vote and income, 68-00.dta")

# Create a common id again.
income_hdi_df <- income_df %>% 
  left_join(hdi_df, by = c("st_state" = "state"))

# Scatterplots.
income_hdi_df %>% 
  filter(st_year == 2000) %>%
  ggplot() +
  geom_text(mapping = aes(x = st_income, y = hdi, label = st_stateabb))
  # geom_point(mapping = aes(x = st_income, y = hdi))

# (3) Names plots ==============================================================

# Names plot.
names_df <- read.csv("data/ROS-Examples-master/Names/data/allnames_clean.csv") %>% 
  as_tibble()

# Make it long and filter.
names_long_df <- names_df %>% 
  pivot_longer(cols = c(-X, -name, -sex), names_to = "year", values_to = "number",
               names_prefix = "X") %>% 
  mutate(year = as.numeric(year),
         last_letter = str_sub(name, start = -1)) 

# Figure 2.7.
names_long_df %>% 
  filter(year == 1906 | year == 1956 | year == 2006,
         sex == "M") %>% 
  group_by(year, last_letter) %>% 
  summarise(counts = sum(number)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(total_counts = sum(counts),
         prop_counts  = counts/total_counts) %>% 
  ungroup() %>% 
  ggplot(data = .) +
  geom_col(mapping = aes(x = last_letter, y = prop_counts,
                         fill = as.factor(year)), 
           position = "dodge") +
  facet_wrap(~year)

# Line graph.
names_long_df %>% 
  filter(sex == "M") %>%
  group_by(year, last_letter) %>% 
  summarize(yearly_counts = sum(number)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(year_total  = sum(yearly_counts),
         yearly_prop = yearly_counts/year_total,
         end_letter  = if_else(last_letter == "d" |
                               last_letter == "y" |
                               last_letter == "n",
                               last_letter,
                               "(other)")) %>% 
  ungroup() %>% 
  ggplot(data = .) +
  geom_line(mapping = aes(x = year, y = yearly_prop, group = last_letter,
                          colour = end_letter)) +
  scale_colour_manual(values = c("grey80", "black", "tomato", "dodgerblue")) 

# (4) Visuals from Chapter 1 regression ========================================

# Set seed.
set.seed(1612)

# Create synthetic data.
N <- 100
pretreat_x <- rnorm(N, 0, 1)^2
treat_x <- rep(0:1, N/2)
pretreat_x <- ifelse(treat_x==0, rnorm(N, 0, 1.2)^2, rnorm(N, 0, .8)^2)
y <- rnorm(N, 20 + 5*pretreat_x + 10*treat_x, 3)
sim_data <- data.frame(pretreat_x, treat_x, y) %>% 
  as_tibble() %>% 
  mutate(treat_x = as.factor(treat_x),
         pretreat_x = round(pretreat_x, 3))

# View it.
glimpse(sim_data)
head(sim_data)

# Basic mean difference between treatment-control groups in Y.
sim_data %>% 
  group_by(treat_x) %>% 
  summarize(mean_values = mean(y))

# Plot of treatment-control differences in the Y.
# We add a mean standard error from bootstrap.
ggplot(data = sim_data,
       mapping = aes(x = treat_x, y = y)) +
  # geom_boxplot(fill = NA) +
  geom_point(alpha = 1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "tomato", linewidth = 1) 

# Simple test of treatment-control differences in the Y.
lm_1 <- lm(y ~ treat_x, data = sim_data)

# Results.
summary(lm_1) 
# tidy(lm_1)

# Pull out information.
lm_1$coefficients

# But what's the relationship between the pre-treatment and Y?
ggplot(data = sim_data) +
  geom_point(mapping = aes(x = pretreat_x, y = y))

# Assessing the relationship between pre-treatment and Y while grouping for
# the actual treatment.
ggplot(data = sim_data,
       mapping = aes(x = pretreat_x, y = y, colour = treat_x)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) 

# Run linear model predicting Y using the treatment X while controlling for
# the pre-treatment X.
lm_2 <- lm(y ~ pretreat_x + treat_x, data=sim_data)

# Summaries.
summary(lm_2)

# Coefficients.
lm2_coef <- round(coefficients(lm_2), 2)

# Plot based on the regression. 
basic_gg <- ggplot(data = sim_data) +
  geom_point(mapping = aes(x = pretreat_x, y = y, colour = treat_x), alpha = 0.6) +
  # Add regression lines.
  geom_abline(intercept = lm2_coef[["(Intercept)"]],
              slope     = lm2_coef[["pretreat_x"]],
              linetype  = "solid") +
  geom_abline(intercept = lm2_coef[["(Intercept)"]] + lm2_coef[["treat_x1"]],
              slope     = lm2_coef[["pretreat_x"]],
              linetype  = "solid") 

basic_gg

# Add some labels. Completely bananas way of doing it, probably.

# First, define the value label position.
x_label <- max(sim_data$pretreat_x)/2

# Then add them.
basic_gg +
  geom_segment(x = x_label,
               xend = x_label,
               y    = lm2_coef[["(Intercept)"]] + x_label*lm2_coef[["pretreat_x"]],
               yend = lm2_coef[["(Intercept)"]] + x_label*lm2_coef[["pretreat_x"]] + coefficients(lm_2)[["treat_x1"]],
               arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
  geom_text(x = x_label*1.4,
            y = lm2_coef[["(Intercept)"]] + x_label*lm2_coef[["pretreat_x"]]*0.9,
            label = paste("Est. effect:", lm2_coef[["treat_x1"]]))

# End.
