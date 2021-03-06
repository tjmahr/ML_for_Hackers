# File-Name:       email_classify_R
# Date:            2012-02-10
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         Code for Chapter 3. In this case we introduce the notion of binary classification.
#                   In machine learning this is a method for determining what of two categories a
#                   given observation belongs to.  To show this, we will create a simple naive Bayes
#                   classifier for SPAM email detection, and visualize the results.
# Data Used:       Email messages contained in data/ directory, source: http://spamassassin.apache.org/publiccorpus/
# Packages Used:   tm, ggplot2

# All source code is copyright (c) 2012, under the Simplified BSD License.
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.



# What we're doing, briefly:

# "At its core, text classification is a 20th-century application of the
# 18th-century concept of _conditional probability_. A conditional probability
# is the likelihood of observing one thing given some other thing that we
# already know about." (p. 77).

# "The text classification algorithm we’re going to use in this chapter, called
# the Naive Bayes classifier, looks for differences of this sort by searching
# through text for words that are either (a) noticeably more likely to occur in
# spam messages, or (b) noticeably more likely to occur in ham messages. When a
# word is noticeably more likely to occur in one context rather than the other,
# its occurrence can be diagnostic of whether a new message is spam or ham. The
# logic is simple: if you see a single word that’s more likely to occur in spam
# than ham, that’s evidence that the email as a whole is spam. If you see many
# words that are more likely to occur in spam than ham and very few words that
# are more likely to occur in ham than spam, that should be strong evidence that
# the email as a whole is spam." (p. 77).

# "Ultimately, our text classifier formalizes this intuition by computing (a)
# the probability of seeing the exact contents of an email conditioned on the
# email being assumed to be spam, and (b) the probability of seeing the same
# email’s contents conditioned on the email being assumed to be ham. If it’s
# much more likely that we would see the email in question if it were spam,
# we’ll declare it to be spam." (p. 77).


# Priors as base rate:

# "How much more likely a message needs to be to merit being labeled spam
# depends upon an additional piece of information: the base rate of seeing spam
# messages. This base rate information is usually called the _prior_. [...] When
# working with email, the prior comes into play because the majority of email
# sent is spam, which means that even weak evidence that an email is spam can be
# sufficient to justify labeling it as spam." (p. 77--78).


# Why the name Naive Bayes?

# "To compute the probability of an email, we will assume that the occurrence
# counts for every word can be estimated in isolation from all of the other
# words. Formally, this amounts to an assumption often referred to as
# statistical independence. When we make this assumption without being certain
# that it’s correct, we say that our model is naive. Because we will also make
# use of base rate information about emails being spam, the model will be also
# called a Bayes model—in homage to the 18thcentury mathematician who first
# described conditional probabilities. Taken together, these two traits make our
# model a Naive Bayes classifier." (p. 78).

library("tm")
library("ggplot2")
library("dplyr")
library("tibble")
source("./03-Classification/email_helpers.R")

# Set the global paths
data_path <- file.path("./", "03-Classification", "data")
figure_path <- file.path("./", "03-Classification", "images/")

spam_path <- file.path(data_path, "spam")
spam2_path <- file.path(data_path, "spam_2")
easyham_path <- file.path(data_path, "easy_ham")
easyham2_path <- file.path(data_path, "easy_ham_2")
hardham_path <- file.path(data_path, "hard_ham")
hardham2_path <- file.path(data_path, "hard_ham_2")



# Create motivating plot
x <- runif(1000, 0, 40)
y1 <- cbind(runif(100, 0, 10), 1)
y2 <- cbind(runif(800, 10, 30), 2)
y3 <- cbind(runif(100, 30, 40), 1)

val <- data.frame(cbind(x, rbind(y1, y2, y3)), stringsAsFactors = TRUE)

ex1 <- ggplot(val) +
  aes(x, V2) +
  geom_jitter(aes(shape = as.factor(V3)),
              position = position_jitter(height = 2)) +
  scale_shape_discrete(guide = "none", solid = FALSE) +
  geom_hline(yintercept = c(10, 30), linetype = 2) +
  theme_bw() +
  xlab("X") +
  ylab("Y")
ex1

ggsave(
  plot = ex1,
  filename =  "00_Ex1.pdf",
  path = figure_path,
  height = 10,
  width = 10)





# With all of our support functions written, we can perform the classification.
# First, we create document corpus for spam messages

# Get all the SPAM-y email into a single vector
spam_docs <- dir(spam_path, full.names = TRUE)
all_spam <- spam_docs %>% lapply(get_msg) %>% unlist

# Create a DocumentTermMatrix from that vector
spam_tdm <- get_tdm(all_spam)
spam_df <- convert_tdm_to_training_df(spam_tdm)

spam_df %>% arrange(desc(occurrence))

# html-centric terms
spam_df %>%
  filter(term %in% c("body", "table", "font", "html", "head"))


# Now do the same for the EASY HAM email
easyham_docs <- easyham_path %>%
  dir(full.names = TRUE) %>%
  head(500)
all_easyham <- easyham_docs %>%
  lapply(get_msg) %>%
  unlist

easyham_tdm <- get_tdm(all_easyham)
easyham_df <- convert_tdm_to_training_df(easyham_tdm)

easyham_df %>% arrange(desc(occurrence))





# "To calculate the conditional probability of a message, we combine the
# probabilities of each term in the training data by taking their product. For
# example, if the frequency of seeing `html` in a spam message is 0.30 and the
# frequency of seeing `table` in a spam message is 0.10, then we’ll say that the
# probability of seeing both in a spam message is 0.30 × 0.10 = 0.03. But for
# those terms in the email that are not in our training data, we have no
# information about their frequency in either spam or ham messages. [...] For
# our purposes, we will use a very simple rule: assign a very small probability
# to terms that are not in the training set. This is, in fact, a common way of
# dealing with missing terms in simple text classifiers, and for our purposes it
# will serve just fine. In this exercise, by default we will set this
# probability to 0.0001%, or one-ten-thousandth of a percent, which is
# sufficiently small for this data set. Finally, because we are assuming that
# all emails are equally likely to be ham or spam, we set our default prior
# belief that an email is of some type to 50%." (p. 85).


# This is the our workhorse function for classifying email.  It takes
# two required paramters: a file path to an email to classify, and
# a data frame of the trained data.  The function also takes two
# optional parameters.  First, a prior over the probability that an email
# is SPAM, which we set to 0.5 (naive), and constant value for the
# probability on words in the email that are not in our training data.
# The function returns the naive Bayes probability that the given email
# is spam_
classify_email <- function(path, training_df, prior = 0.5, c = 1e-4) {
  # Get a data-frame with one row per term from email message
  msg_df <- get_msg(path) %>%
    get_tdm %>%
    convert_tdm_to_df %>%
    select(term)

  # Attach training data.
  msg_df <- msg_df %>%
    left_join(training_df, by = "term") %>%
    # fallback occurruence if a term didn't have a match in training data.
    tidyr::replace_na(list(occurrence = c))

  prior * prod(msg_df$occurrence)
}

# "You may note that there are actually 2,500 ham emails in this directory. So
# why are we ignoring four-fifths of the data? When we construct our first
# classifier, we will assume that each message has an equal probability of being
# ham or spam. As such, it is good practice to ensure that our training data
# reflects our assumptions. We only have 500 spam messages, so we will limit or
# ham training set to 500 messages as well." (p. 84.)


# Run classifer against HARD HAM
hardham_docs <- hardham_path %>%
  dir(full.names = TRUE)

hardham_spamtest <- hardham_docs %>%
  lapply(classify_email, training_df = spam_df) %>%
  unlist

hardham_hamtest <- hardham_docs %>%
  lapply(classify_email, training_df = easyham_df) %>%
  unlist

hardham_res <- hardham_spamtest > hardham_hamtest
summary(hardham_res)




# Find counts of just terms 'html' and 'table' in all SPAM and EASYHAM docs, and
# create figure.
html_spam <- spam_docs %>% lapply(count_word, search_term = "html") %>% unlist
table_spam <- spam_docs %>% lapply(count_word, search_term = "table") %>% unlist

spam_init <- data_frame(
  html = html_spam,
  table = table_spam,
  type = "SPAM")

html_easyham <- easyham_docs %>% lapply(count_word, "html") %>% unlist
table_easyham <- easyham_docs %>% lapply(count_word, "table") %>% unlist

easyham_init <- data_frame(
  html = html_easyham,
  table = table_easyham,
  type = "SPAM")

init_df <- bind_rows(spam_init, easyham_init)

init_plot1 <- ggplot(init_df) +
  aes(x = html, y = table) +
  geom_point(aes(shape = type)) +
  scale_shape_manual(values = c("SPAM" = 1, "EASYHAM" = 3)) +
  labs(x = "Frequency of 'html'",
       y = "Freqeuncy of 'table'",
       shape = "Email Type") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

init_plot1

ggsave(
  plot = init_plot1,
  file = "01_init_plot1.pdf",
  path = figure_path,
  width = 10,
  height = 10)

init_plot2 <- ggplot(init_df) +
  aes(x = html, y = table) +
  geom_point(aes(shape = type), position = "jitter") +
  scale_shape_manual(values = c("SPAM" = 1, "EASYHAM" = 3)) +
  labs(x = "Frequency of 'html'",
       y = "Freqeuncy of 'table'",
       shape = "Email Type") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()
init_plot2

ggsave(
  plot = init_plot2,
  file = "02_init_plot2.pdf",
  path = figure_path,
  width = 10,
  height = 10)

# Finally, attempt to classify the HARDHAM data using the classifer developed above.
# The rule is to classify a message as SPAM if Pr(email) = SPAM > Pr(email) = HAM
spam_classifier <- function(path) {
  pr_spam <- classify_email(path, spam_df)
  pr_ham <- classify_email(path, easyham_df)
  data_frame(
    pr_spam = pr_spam,
    pr_ham = pr_ham,
    label = ifelse(pr_spam > pr_ham, "Spam", "Ham"))
}

# Get lists of all the email messages
easyham2_docs <- dir(easyham2_path, full.names = TRUE)
hardham2_docs <- dir(hardham2_path, full.names = TRUE)
spam2_docs <- dir(spam2_path, full.names = TRUE)

# Classify them all!
easyham2_class <- easyham2_docs %>% lapply(spam_classifier) %>% bind_rows
hardham2_class <- hardham2_docs %>% lapply(spam_classifier) %>% bind_rows
spam2_class <- spam2_docs %>% lapply(spam_classifier) %>% bind_rows

easyham2_final <- easyham2_class %>% mutate(type = "EASYHAM")
hardham2_final <- hardham2_class %>% mutate(type = "HARDHAM")
spam2_final <- spam2_class %>% mutate(type = "SPAM")

class_df <- bind_rows(easyham2_final, hardham2_final, spam2_final)
class_df %>%
  mutate(correct_type = ifelse(type == "SPAM", "Spam", "Ham"),
         correct = label == correct_type) %>%
  group_by(type, correct_type) %>%
  summarise(mean(correct))

class_df %>%
  group_by(type, label) %>%
  count

# Create final plot of results
class_plot <- ggplot(class_df) +
  aes(x = log(pr_ham), log(pr_spam)) +
  geom_point(aes(shape = type, alpha = 0.5)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_shape_manual(values = c("EASYHAM" = 1, "HARDHAM" = 2, "SPAM" = 3),
                     name = "Email Type") +
  scale_alpha(guide = "none") +
  xlab("log[Pr(HAM)]") +
  ylab("log[Pr(SPAM)]") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
class_plot


# "There appear to be two general ways it is failing. First, there are many hard
# ham messages that have a positive probability of being spam but a near-zero
# probability of being ham. These are the points pressed up against the y-axis.
# Second, there are both easy and hard ham messages that have a much higher
# relative probability of being ham. Both of these observations may indicate a
# weak training data set for ham emails, as there are clearly many more terms
# that should be associated with ham that currently are not." (p. 90).

ggsave(
  plot = class_plot,
  file = "03_final_classification.pdf",
  path = figure_path,
  height = 10,
  width = 10)

get_results <- function(bool_vector) {
  results <- c(length(bool_vector[which(bool_vector == FALSE)]) / length(bool_vector),
               length(bool_vector[which(bool_vector == TRUE)]) / length(bool_vector))
  return(results)
}




# Save results as a 2x3 table
easyham2_col <- get_results(subset(class_df, Type == "EASYHAM")$Class)
hardham2_col <- get_results(subset(class_df, Type == "HARDHAM")$Class)
spam2_col <- get_results(subset(class_df, Type == "SPAM")$Class)

class.res <- rbind(easyham2_col, hardham2_col, spam2_col)
colnames(class.res) <- c("NOT SPAM", "SPAM")
print(class.res)

# Save the training data for use in Chapter 4
write.csv(spam_df, file.path("./", "03-Classification", "data", "spam_df.csv"), row.names = FALSE)
write.csv(easyham_df, file.path("./", "03-Classification", "data", "easyham_df.csv"), row.names = FALSE)
