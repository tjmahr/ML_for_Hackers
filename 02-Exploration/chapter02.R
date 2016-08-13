# File-Name:       chapter02.R
# Date:            2011-11-11
# Author:          John Myles White
# Email:           jmw@johnmyleswhite.com
# Purpose:         Code for Chapter 2. Showcases tools for exploratory data analysis.
# Data Used:       data/01_heights_weights_genders.csv
# Packages Used:   ggplot2
# Machine:         John Myles White's MacBook

# All source code is copyright (c) 2011, under the Simplified BSD License.
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.




# "Because humans are vulnerable to discovering patterns that won’t stand up to
# careful scrutiny, the exploratory step in data analysis can’t exist in
# isolation; it needs to be accompanied by a confirmatory step. Think of
# confirmatory data analysis as a sort of mental hygiene routine that we use to
# clean off our beliefs about the world after we’ve gone slogging through the
# messy—and sometimes lawless—world of exploratory data visualization."
# (p. 29)

# "Confirmatory data analysis usually employs two tools:
#
# * Testing a formal model of the pattern that you think you’ve found on a new
#   data set that you didn’t use to find the pattern.
# * Using probability theory to test whether the patterns you’ve found in your
#   original data set could reasonably have been produced by chance."
# (p. 30)

# "It’s complicated to define the mode of an arbitrary vector because you need
# the numbers in the vector to repeat if you’re going to define the mode
# numerically. When the numbers in a vector could be arbitrary floating-point
# values, it’s unlikely that any single numeric value would ever be repeated in
# the vector. For that reason, modes are only really defined visually for many
# kinds of data sets."
# (p. 40)


# "Beyond showing you the tools for visualizing your data, we’ll also describe
# some of the canonical shapes you can expect to see when you start looking at
# data. These idealized shapes, also called distributions, are standard patterns
# that statisticians have studied over the years. When you find one of these
# shapes in your data, you can often make broad inferences about your data: how
# it originated, what sort of abstract properties it will have, and so on." (p.
# 44).


# Load in the data set from disk.
data_file <- file.path(".", "02-Exploration", "data",
                       "01_heights_weights_genders.csv")
heights_weights <- readr::read_csv(data_file)

# Create a numeric vector containing just the heights data.
heights <- with(heights_weights, Height)
summary(heights)

# Expected output.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#54.26   63.51   66.32   66.37   69.17   79.00

# Define our own mean and median functions.
my_mean <- function(x) {
  sum(x) / length(x)
}

my_median <- function(x) {
  halfway <- length(x) / 2
  sorted_x <- sort(x)
  if (length(x) %% 2 == 0) {
    indices <- c(halfway, halfway + 1)
    median <- mean(sorted_x[indices])
  } else {
    index <- ceiling(halfway)
    median <- sorted_x[index]
  }
  median
}

# Compare means and medians on toy examples.
my_vector <- c(0, 100)

my_vector
# [1]	0 100

mean(my_vector)
#[1] 50

median(my_vector)
#[1] 50

my_vector <- c(0, 0, 100)

mean(my_vector)
#[1] 33.33333

median(my_vector)
#[1] 0

# Confirm that our mean and median functions produce the correct answer.
my_mean(heights)
#[1] 66.36756

my_median(heights)
#[1] 66.31807

mean(heights) - my_mean(heights)
#[1] 0

median(heights) - my_median(heights)
#[1] 0

# Experiment with functions for assessing the range of a data set.
min(heights)
#[1] 54.26313

max(heights)
#[1] 78.99874

c(min(heights), max(heights))
#[1] 54.26313 78.99874

range(heights)
#[1] 54.26313 78.99874

# "Another way of thinking of these numbers is to think of the `min` as the
# number that 0% of your data is below and the `max` as the number that 100% of
# your data is below. Thinking that way leads to a natural extension: how can
# you find the number that N% of your data is below? The answer to that question
# is to use the `quantile` function in R. The Nth quantile is exactly the number
# that N% of your data is below."
# (p. 40)

# Try out the quantile function for computing arbitrary quantiles.
quantile(heights)
#      0%      25%      50%      75%     100%
#54.26313 63.50562 66.31807 69.17426 78.99874


quantile(heights, probs = seq(0, 1, by = 0.20))
#      0%      20%      40%      60%      80%     100%
#54.26313 62.85901 65.19422 67.43537 69.81162 78.99874

seq(0, 1, by = 0.20)
#[1] 0.0 0.2 0.4 0.6 0.8 1.0

# Define a variance function to assess the spread of data.
my_var <- function(x) {
  m <- mean(x)
  sum((x - m) ^ 2) / length(x)
}

# Test our variance function for correctness.
my_var(heights) - var(heights)

# Update the variance function to make it unbiased.
my_var <- function(x) {
  m <- mean(x)
  sum((x - m) ^ 2) / (length(x) - 1)
}

# Test our variance function again for correctness.
my_var(heights) - var(heights)

# Check the range predicted by the variance function.
c(mean(heights) - var(heights), mean(heights) + var(heights))
#[1] 51.56409 81.17103

c(mean(heights) - var(heights), mean(heights) + var(heights))
#[1] 51.56409 81.17103
range(heights)
#[1] 54.26313 78.99874

# Switch to standard deviations instead for thinking about ranges.
my_sd <- function(x) {
  sqrt(my_var(x))
}

# Test our standard deviation function for correctness.
my_sd(heights) - sd(heights)

c(mean(heights) - sd(heights), mean(heights) + sd(heights))
# [1] 62.52003 70.21509

range(heights)
#[1] 54.26313 78.99874

c(mean(heights) - sd(heights), mean(heights) + sd(heights))
# [1] 62.52003 70.21509

c(quantile(heights, probs = 0.25), quantile(heights, probs = 0.75))
#     25%      75%
#63.50562 69.17426



# Start visualizing data using the ggplot2 package.
library("ggplot2")

# "Immediately, something should jump out at you: there’s a bell curve shape in
# your data. Most of the entries are in the middle of your data, near the mean
# and median height. But there’s a danger that this shape is an illusion caused
# by the type of histogram we’re using. One way to check this is to try using
# several other binwidths. This is something you should always keep in mind when
# working with histograms: the binwidths you use impose _external_ structure on
# your data at the same time that they reveal _internal_ structure in your
# data." (p. 44).

# Load the data from scratch for purity.
heights_weights <- read.csv(data_file, header = TRUE, sep = ",")

# Experiment with histograms.
base_plot <- ggplot(heights_weights) + aes(x = Height)

base_plot + geom_histogram(binwidth = 1)
base_plot + geom_histogram(binwidth = 5)
base_plot + geom_histogram(binwidth = 0.01)



# Experiment with kernel density estimates.
base_plot + geom_density()

# Separate out heights and weights based on gender.
ggplot(heights_weights) +
  aes(x = Height, fill = Gender) +
  geom_density()

ggplot(heights_weights) +
  aes(x = Weight, fill = Gender) +
  geom_density()

# Produce two facets in a single plot to make it easier to see the hidden
# structure.
ggplot(heights_weights) +
  aes(x = Weight, fill = Gender) +
  geom_density() +
  facet_grid(Gender ~ .) +
  guides(fill = FALSE)




# Experiment with random numbers from the normal distribution.
m <- 0
s <- 1
ggplot(data.frame(X = rnorm(100000, m, s)), aes(x = X)) +
  geom_density()


# Compare the normal distribution with the Cauchy distribution.
set.seed(1)
normal_values <- rnorm(250, 0, 1)
cauchy_values <- rcauchy(250, 0, 1)
range(normal_values)
range(cauchy_values)

ggplot(data.frame(X = normal_values), aes(x = X)) +
  geom_density()
ggplot(data.frame(X = cauchy_values), aes(x = X)) +
  geom_density()

# Experiment with random numbers from the gamma distribution.
gamma_values <- rgamma(100000, 1, 0.001)
ggplot(data.frame(X = gamma_values), aes(x = X)) +
  geom_density()

# Generate scatterplots of the heights and weights to see their relationship.
ggplot(heights_weights, aes(x = Height, y = Weight)) +
  geom_point()

# Add a smooth shape that relates the two explicitly.
ggplot(heights_weights, aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()

# See how the smooth shape gets better with more data.
ggplot(heights_weights[1:20, ], aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()

last_plot() %+% heights_weights[1:200, ]
last_plot() %+% heights_weights[1:2000, ]

# Visualize how gender depends on height and weight.
ggplot(heights_weights, aes(x = Height, y = Weight)) +
  geom_point(aes(color = Gender, alpha = 0.25)) +
  scale_alpha(guide = "none") +
  scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
  theme_bw()

# An alternative using bright colors.
ggplot(heights_weights, aes(x = Height, y = Weight, color = Gender)) +
  geom_point()

heights_weights <- transform(heights_weights,
                             Male = ifelse(Gender == "Male", 1, 0))

logit_model <- glm(Male ~ Weight + Height,
                   data = heights_weights,
                   family = binomial(link = "logit"))

ggplot(heights_weights, aes(x = Height, y = Weight)) +
  geom_point(aes(color = Gender, alpha = 0.25)) +
  scale_alpha(guide = "none") +
  scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
  theme_bw() +
  geom_abline(intercept = -coef(logit_model)[1] / coef(logit_model)[2],
              slope = -coef(logit_model)[3] / coef(logit_model)[2],
              color = "black")
