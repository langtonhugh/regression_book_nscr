# Load libraries.
library(rosdata)
library(dplyr)
library(readr)
library(ggplot2)
library(rstanarm)

# Explore data.
glimpse(hibbs)

# Replicate the plot.
ggplot(data = hibbs,
       mapping = aes(x = growth, y = vote)) +
  # geom_label(mapping = aes(label = year), nudge_x = 0.3, fill = NA, size = 3) +
  geom_point() 

# Run model.
M1 <- stan_glm(vote ~ growth, data=hibbs)

# Model summary.
M1

# Print slope.
coef(M1)

# Add line to plot.
ggplot(data = hibbs,
       mapping = aes(x = growth, y = vote)) +
  geom_point() +
  geom_abline(slope     = coef(M1)[["growth"]],
              intercept = coef(M1)[["(Intercept)"]])

# Peacekeeping data.
peace_df <- read_csv("data/ROS-Examples-master/Peacekeeping/minidata.csv")

# This one is weird: badness var is ethnicity?!
# peace_df <- read_csv("data/ROS-Examples-master/Peacekeeping/data/minidata.csv")

# Explore.
glimpse(peace_df)

# Create date measure. It's actually the same as delay. 
peace_df <- peace_df %>% 
  mutate(time_diff = (faildate-cfdate)/365)

# Plot. 
peace_df %>% 
  ggplot(data = .) +
  geom_histogram(mapping = aes(x = delay), bins = 10) +
  facet_wrap(~`peacekeepers?`)

# Scatter.
ggplot(data = peace_df) +
  geom_point(mapping = aes(y = delay,
                           colour = as.factor(`censored?`),
                           x = badness,
                           )) +
  facet_wrap(~`peacekeepers?`)

# Means.
peace_df %>% 
  group_by(`peacekeepers?`, `censored?`) %>% 
  summarise(mean_badness = mean(badness, na.rm = TRUE))
  
# Simple causal graph.
# for reproducibility of simulated data
SEED <- 1151
set.seed(SEED)
N <- 50
x <- runif(N, 1, 5)
y <- rnorm(N, 10 + 3*x, 3)
x_binary <- ifelse(x<3, 0, 1)
causal_df <- data.frame(N, x, y, x_binary)

# Plot.
ggplot(data = causal_df) +
  geom_point(mapping = aes(y = y, x = x))


