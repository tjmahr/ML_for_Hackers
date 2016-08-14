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
library("dplyr")
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
base_plot + geom_histogram(binwidth = 5) + ggtitle("oversmoothing")
base_plot + geom_histogram(binwidth = 0.01) + ggtitle("undersmoothing")



# "Density plots for large data sets look a lot more like the theoretical shapes
# we expect to find in our data. Additionally, density plots have some
# theoretical superiority over histograms: in theory, using a density plot
# should require fewer data points to reveal the underlying shape of your data
# than a histogram." (p. 46).

# Experiment with kernel density estimates.
base_plot + geom_density()

# The top of the curve is too flat to be normally distributed. But we know
# what's wrong...

# Separate out heights and weights based on gender.
ggplot(heights_weights) +
  aes(x = Height, fill = Gender) +
  geom_density()

ggplot(heights_weights) +
  aes(x = Weight, fill = Gender) +
  geom_density()

# "In future chapters, we’ll cover this sort of mixture of bell curves in some
# detail, but it’s worth giving a name to the structure we’re looking at right
# now: it’s a mixture model in which two standard distributions have been mixed
# to produce a nonstandard distribution." (p. 48).

# Produce two facets in a single plot to make it easier to see the hidden
# structure.
ggplot(heights_weights) +
  aes(x = Weight, fill = Gender) +
  geom_density() +
  facet_grid(Gender ~ .) +
  guides(fill = FALSE)


# "As we said earlier, the mode of a continuous list of numbers isn’t well
# defined, because no numbers repeat. But the mode has a clear visual
# interpretation: when you make a density plot, the mode of the data is the peak
# of the bell." (p. 52).

# Experiment with random numbers from the normal distribution.
m <- 0
s <- 1
ggplot(data.frame(X = rnorm(100000, m, s)), aes(x = X)) +
  geom_density()

# "The normal distribution has a single mode, which is also the mean and the
# median." (p. 52).

# We characterize distributions by number of modes, whether it is symmetric or
# skewed or how thick the tails of the distributions are.

# "The normal distribution, for example, produces values that are no more than
# three standard deviations away from the mean about 99% of the time. In
# contrast, another bell-shaped distribution called the Cauchy distribution
# produces only 90% of its values inside those three standard deviation bounds.
# And, as you get further away from the mean value, the two types of
# distributions become even more different: a normal distribution almost never
# produces values that are six standard deviations away from the mean, whereas a
# Cauchy will do it almost 5% of the time." (p.54--55).

# Compare the normal distribution with the Cauchy distribution.
set.seed(1)
normal_values <- rnorm(250, 0, 1)
cauchy_values <- rcauchy(250, 0, 1)
range(normal_values)
range(cauchy_values)

plot_dist <- function(xs) {
  ggplot(data_frame(X = xs)) +
    aes(x = X) +
    geom_density() +
    ggtitle(lazyeval::expr_text(xs))
}

plot_dist(normal_values)
plot_dist(cauchy_values)


# Create canonical cauchy-vs-normal plot
normals <- data_frame(X = rnorm(250000, 0, 1), type = "Normal")
cauchys <- data_frame(X = rcauchy(250000, 0, 1), type = "Cauchy")
both_types <- dplyr::bind_rows(normals, cauchys)

ggplot(both_types) +
  aes(x = X, linetype = type) +
  geom_density() +
  xlim(-3, 3)

# Use the example from the help page ?rstanarm::priors
compare_priors <- function(scale = 1, df_t = 2, xlim = c(-10, 10)) {
  dt_loc_scale <- function(x, df, location, scale) {
    # t distribution with location & scale parameters
    1 / scale * dt((x - location) / scale, df)
  }
  ggplot(data.frame(x = xlim), aes(x)) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = scale),
                  color = "purple", size = .75) +
    stat_function(fun = dt_loc_scale,
                  args = list(df = df_t, location = 0, scale = scale),
                  color = "orange", size = .75) +
    stat_function(fun = dcauchy, args = list(location = 0, scale = scale),
                  color = "skyblue", size = .75, linetype = 2) +
    ggtitle("normal (purple) vs student_t (orange) vs cauchy (blue)")
}
# Cauchy has fattest tails, then student_t, then normal
compare_priors()

# "Let’s summarize the qualitative properties of the normal once more: it’s
# unimodal, it’s symmetric, and it has a bell shape with thin tails. The Cauchy
# is unimodal and symmetric, and it has a bell shape with heavy tails. After the
# normal distribution, there are two more canonical images we want to show you
# before we bring this section on density plots to a close: a mildly skewed
# distribution called the _gamma_ and a very skewed distribution called the
# _exponential_." (p. 56-57).

# Experiment with random numbers from the gamma distribution.
plot_dist(rgamma(n = 100000, shape = 5, rate = .5))
plot_dist(rgamma(n = 100000, shape = 1, rate = 0.001))

# They show an example of high scores in a videogame as a gamma like
# distribution.

# "Because the mode of the exponential distribution occurs at zero, it’s almost
# like you had cut off the positive half of a bell to produce the exponential
# curve. This distribution comes up quite a lot when the most frequent value in
# your data set is zero and only positive values can ever occur. For example,
# corporate call centers often find that the length of time between the calls
# they receive looks like an exponential distribution." (p. 59--60).

plot_dist(rexp(100000, rate = .001))
plot_dist(rexp(100000, rate = .1))





# Generate scatterplots of the heights and weights to see their relationship.
ggplot(heights_weights) +
  aes(x = Height, y = Weight) +
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
ggplot(heights_weights) +
  aes(x = Height, y = Weight) +
  geom_point(aes(color = Gender, alpha = 0.25)) +
  scale_alpha(guide = FALSE) +
  scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
  theme_bw()

# An alternative using bright colors.
ggplot(heights_weights) +
  aes(x = Height, y = Weight, color = Gender) +
  geom_point()




# Separating hyperplane
heights_weights <- heights_weights %>%
  mutate(Male = Gender == "Male")

logit_model <- glm(
  Male ~ Weight + Height,
  data = heights_weights,
  family = binomial(link = "logit"))

summary(logit_model)

ggplot(heights_weights) +
  aes(x = Height, y = Weight) +
  geom_point(aes(color = Gender), alpha = 0.25) +
  scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
  scale_alpha(guide = FALSE) +
  theme_bw() +
  geom_abline(intercept = -coef(logit_model)[1] / coef(logit_model)[2],
              slope = -coef(logit_model)[3] / coef(logit_model)[2],
              color = "black")
