# original header:

# File-Name:       ufo_sightings.R
# Date:            2012-02-10
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         Code for Chapter 1.  In this case we will review some of the basic
#                  R functions and coding paradigms we will use throughout this book.
#                  This includes loading, viewing, and cleaning raw data; as well as
#                  some basic visualization.  This specific case we will use data from
#                  reported UFO sightings to investigate what, if any, seasonal trends
#                  exists in the data.
# Data Used:       http://www.infochimps.com/datasets/60000-documented-ufo-sightings-with-text-descriptions-and-metada

# All source code is copyright (c) 2012, under the Simplified BSD License.
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative
# Commons Attribution-Share Alike 3.0 United States License:
# http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# /original header




# "[To demonstrate basic R techniques,] we will address a question with pure
# entertainment value. Recently, the data service Infochimps.com released a data
# set with over 60,000 documented reports of unidentified flying object (UFO)
# sightings. The data spans hundreds of years and has reports from all over the
# world. Though it is international, the majority of sightings in the data come
# from the United States. With the time and spatial dimensions of the data, we
# might ask the following questions: are there seasonal trends in UFO sightings;
# and what, if any, variation is there among UFO sightings across the different
# states in the US?" (p. 12)


# Load libraries and data
library(ggplot2)    # We'll use ggplot2 for all of our visualizations
library(scales)     # We'll need to fix date formats in plots
library(dplyr)      # For data manipulation
library(stringr)    # For string manipulation

# The data file we are going to load doesn't have the column names in the first
# row. These are the field names from the data's description file. We will set
# the column names accordingly as we read in the data.
ufo_col_names <- c("DateOccurred", "DateReported", "Location",
                   "ShortDescription", "Duration", "LongDescription")

# We use the readr package because it is fast, never sets strings to factors,
# prints out messages about inconsistent rows or columns, and returns the data
# as a "tibble" dataframe (which has a better print method).
ufo <- readr::read_tsv(
  file = "./01-Introduction/data/ufo/ufo_awesome.tsv",
  col_types = "cccccc",
  col_names = ufo_col_names)

# (Some of the LongDescription values contained tabs which is why readr warned
# about unexpected columns.)

# readr::read_tsv tries to guess column types as it reads in the data. The
# DateOccurred and DateReported fields store dates in YYYYMMDD format (e.g.,
# 19951009). readr will convert these to integers because they look like
# numbers. By using col_types = "cccccc", we explicitly tell readr to treat all
# 6 columns as [c]haracter vectors

# Inspect the data frame
ufo

ufo <- ufo %>% select(-ShortDescription, -Duration, -LongDescription)

# To work with the dates, we will need to convert the YYYYMMDD string to an R Date
# type using the 'strptime' function

# But, something has gone wrong with the data. For now, we'll just ignore the
# errata by removing those entries that have not parsed correctly.  We know that
# the date strings are always 8 characters long, and any deviation from this
# would indicate a row to ignore.

# Peek at the rows with bad dates
ufo %>% filter(nchar(DateOccurred) != 8)
ufo %>% filter(nchar(DateReported) != 8)

# The book says 371 rows have badly formatted dates. These filters say 254 rows
# do.

# Exclude them
ufo <- ufo %>%
  filter(nchar(DateOccurred) == 8, nchar(ufo$DateReported) == 8)

# Now we can convert the strings to Date objects and work with them properly
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format = "%Y%m%d")

# It will be useful to create separate columns for both town and state from the
# Location column.

# We write a function to take location string and return a data-frame with a
# USCity column and a USState column. We do this by breaking a location string
# at the cpmma "Iowa City, IA" becomes c("Iowa City", " IA").

get_us_location <- function(location) {
  # Break strings at commas.
  pieces <- location %>% str_split(",") %>% unlist

  # No comma case gets NAs
  if (length(pieces) == 1) {
    pieces <- c(NA_character_, NA_character_)
  }

  # Assume piece after final comma will contain the state. Collapse everything
  # but last item into a single string.
  if (2 < length(pieces)) {
    but_last <- pieces %>% head(-1) %>% paste0(collapse = ",")
    last <- tail(pieces, 1)
    pieces <- c(but_last, last)
  }

  # Trim whitespace.
  cleaned <- str_trim(pieces)

  if (!is_us_state(cleaned[2])) {
    cleaned <- c(NA_character_, NA_character_)
  }

  data_frame(USCity = cleaned[1], USState = cleaned[2])
}

# Is a string a US state abbreviation?
is_us_state <- function(x) {
  x %in% datasets::state.abb
}

library("purrr")
city_state <- ufo$Location %>% map_df(get_us_location)
city_state %>% sample_n(100)

# "DC" is not in our list of states, so this location will not be counted
#
# - Washington, D.C., DC





# Add the city and state data to ufo data-frame.
ufo <- bind_cols(ufo, city_state)

# Finally, we'll use 'filter' to examine only events in the United States.
ufo_us <- ufo %>% filter(!is.na(USState))

# number of (possibly) non-US sightings excluded
nrow(ufo) - nrow(ufo_us)

# Now, we are ready to do some analysis! First, take a look at the
# post-processed data
ufo_us

# The summary functions shows us that the data actually go back a very long time
# (1440!).  So, we will want to take a quick look at the date to see where the
# majority of the data exists. We can do this by creating a histogram of
# frequencies for UFO sightings over time.
quick_hist <- ggplot(ufo_us) +
  aes(x = DateOccurred) +
  geom_histogram() +
  scale_x_date(date_breaks = "50 years")
quick_hist

ggsave(
  filename = "quick_hist.pdf",
  plot = quick_hist,
  path = "./01-Introduction/images/",
  height = 6,
  width = 8)

# First, we notice that there are many very old entries in the data.  For our
# purposes, we will only look at incidents that occurred from 1990 to the most
# recent
ufo_us <- ufo_us %>%
  filter(as.Date("1990-01-01") <= DateOccurred)

nrow(ufo_us)

# Let's look at the histogram now
new_hist <- ggplot(ufo_us) +
  aes(x = DateOccurred) +
  geom_histogram(fill = 'white', color = 'red', binwidth = 365, center = 0) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")
new_hist

ggsave(
  filename = "new_hist.pdf",
  plot = new_hist,
  path = "./01-Introduction/images/",
  height = 6,
  width = 8)

# Now that we have the data we want, let's look at some aggregations.  But
# first, we create a column of just the Year-Month of each incident.
ufo_us$YearMonth <- strftime(ufo_us$DateOccurred, format = "%Y-%m")

# This will return the number of sightings of UFO by Year-Month and state for
# the whole time-series
sightings_counts <- ufo_us %>% count(USState, YearMonth) %>% ungroup
sightings_counts

# As we might expect, there are several Year-Month and state combinations for
# which there are no UFO sightings.  We need to count these as zero so we can go
# back and fill those in. First, we will create a new vector that has all of the
# Year-Month dates in it that span the range of our time-series (1990-2010)
date_range <- seq.Date(
  from = as.Date(min(ufo_us$DateOccurred)),
  to = as.Date(max(ufo_us$DateOccurred)),
  by = "month") %>%
  strftime(format = "%Y-%m")

# We create a row for each combination of USState and YearMonth. Instead of
# choosing NA for n, we default to 0.
all_sightings <- sightings_counts %>%
  tidyr::complete(USState, YearMonth = date_range, fill = list(n = 0))
all_sightings

# Now we just need to clean up the merged data frame a bit Set the column names
# to something meaningful.
names(all_sightings) <- c("State", "YearMonth", "Sightings")

# Reset the character Year-Month to a Date objects
all_sightings$YearMonth <- all_sightings$YearMonth %>%
  # Add a dummy day to each entry so a date can be parsed. Just year/month too
  # vague to describe a "date".
  paste0("-01") %>%
  as.Date(format = "%Y-%m-%d")

# Capitalize the State abbreviation and set as factor
# all_sightings$State <- as.factor(all_sightings$State)

# There are lots of ways we could test the seasonality of of these sightings,
# but one basic method is to inspect the trends visually.  We now construct a
# plot that will show these trends for all 50 U.S. states over the time-series.

# First we have to create a ggplot2 object and then create a geom layer, which in this case is a line.
# Additional points of note:
# (1) facet_wrap() will create separate plots for each state on a 10x5 grid.
# (2) theme_bw() changes the default ggplot2 style from grey to white (personal preference).
# (3) scale_x_date() scales the x-axis as a date, with major lines every 5 years.
# (4) xlab() and ylab() set axis labels.

state_plot <- ggplot(all_sightings) +
  aes(x = YearMonth,y = Sightings) +
  geom_line(color = "darkblue") +
  facet_wrap(~ State, nrow = 10, ncol = 5) +
  scale_x_date(date_breaks = "5 years", date_labels = '%Y') +
  theme_bw() +
  xlab("Years") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")
state_plot

ggsave(
  plot = state_plot,
  filename = "ufo_sightings.pdf",
  path = "./01-Introduction/images/",
  width = 14,
  height = 8.5)


# Create a new graph where the number of signtings is normalized by the state
# population.
state_pop <- readr::read_csv(file.path('./01-Introduction/data/census.csv')) %>%
  select(Pop2000 = `2000`, StateName = State)

states <- data_frame(StateName = state.name, State = state.abb)

state_pop <- left_join(state_pop, states) %>% select(State, Pop2000)

all_sightings <- all_sightings %>%
  left_join(state_pop) %>%
  mutate(Sightings_Norm = Sightings / Pop2000) %>%
  select(-Pop2000)

state_plot_norm <- ggplot(all_sightings) +
  aes(x = YearMonth, y = Sightings_Norm) +
  geom_line(color = "darkblue") +
  facet_wrap(~ State, nrow = 10, ncol = 5) +
  theme_bw() +
  scale_x_date(date_breaks = "5 years", date_labels = '%Y') +
  xlab("Years") +
  ylab("Per Capita Number of Sightings (2000 Census)") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")

# Save the plot as a PDF
ggsave(
  plot = state_plot_norm,
  filename = "ufo_sightings_norm.pdf",
  path = "./01-Introduction/images",
  width = 14,
  height = 8.5)



# Bonus. Create a map of sightings.

# Updated projection using tricks from here
# http://juliasilge.com/blog/Evenly-Distributed/
library("maps")
library("mapproj")
library(ggalt)
library("albersusa")
library(ggplot2)
# library(ggthemes)
library(ggalt)
library(scales)
library(rgeos)
library(maptools)
library(albersusa)

us <- usa_composite()
us_map <- fortify(us, region = "name") %>% as.tbl
us_map

by_state <-  all_sightings %>%
  group_by(State) %>%
  summarise(Sightings = sum(Sightings),
            Sightings_Norm = sum(Sightings_Norm)) %>%
  mutate(Sightings_Norm = Sightings_Norm * 10000) %>%
  left_join(states) %>%
  # mutate(region = tolower(StateName)) %>%
  rename(id = StateName)

map_states <- us_map %>%
  left_join(by_state)

ufo_map <- ggplot(map_states) +
  aes(long, lat, group = group) +
  geom_polygon(aes(fill = Sightings_Norm), colour = "black") +
  coord_proj(us_laea_proj) +
  viridis::scale_fill_viridis(labels = scales::comma) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(plot.title = element_text(size = 16, margin = margin(b = 10))) +
  theme(plot.subtitle = element_text(size = 14, margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9, margin = margin(t = -15))) +
  ggthemes::theme_map(base_size = 12) +
  theme(legend.position = c(.8, .25), legend.justification	= c(0, 0),
        legend.background = element_rect(fill = NA)) +
  ggtitle("UFO sightings by U.S. State (1990-2010)",
          subtitle = "Sightings per 10,000 residents. Populations based on 2000 Census.") +
  labs(fill = NULL, caption = "Example from 'Machine Learning for Hackers'.")
ufo_map

ggsave(
  file = "ufo_map.pdf",
  plot = ufo_map,
  width = 10,
  height = 7,
  path = "./01-Introduction/images/")
