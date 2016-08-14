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




# Load libraries
library("tm")
library("ggplot2")
library("dplyr")
library("tibble")
# library("stringr")

# Set the global paths
spam_path <- file.path("./", "03-Classification", "data", "spam")
spam2_path <- file.path("./", "03-Classification", "data", "spam_2")
easyham_path <- file.path("./", "03-Classification", "data", "easy_ham")
easyham2_path <- file.path("./", "03-Classification", "data", "easy_ham_2")
hardham_path <- file.path("./", "03-Classification", "data", "hard_ham")
hardham2_path <- file.path("./", "03-Classification", "data", "hard_ham_2")

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

ggsave(
  plot = ex1,
  filename =  "00_Ex1.pdf",
  path = file.path("./", "03-Classification", "images"),
  height = 10,
  width = 10)




# Return a single element vector of just the email body. This is a very simple
# approach, as we are only using words as features.
get_msg <- function(path) {
  text <- readLines(path, encoding = "latin1")

  # The message always begins after the first full line break
  msg_start <- which(text == "")[1] + 1

  # Return an empty vector is no line break is found
  if (!is.na(msg_start)) {
    msg_lines <- text[seq(msg_start, length(text), 1)]
    msg <- paste(msg_lines, collapse = "\n")
  } else{
    msg <- character(0)
  }

  msg
}

# Create a TermDocumentMatrix (TDM) from the corpus of SPAM email. The TDM
# control can be modified, and the sparsity level can be altered. This TDM is
# used to create the feature set used to do train our classifier.
get_tdm <- function(doc_vec) {
  control <- list(
    stopwords = TRUE,
    removePunctuation = TRUE,
    removeNumbers = TRUE,
    minDocFreq = 2)
  doc_corpus <- tm::Corpus(tm::VectorSource(doc_vec))
  doc_dtm <- tm::TermDocumentMatrix(doc_corpus, control)
  doc_dtm
}

# This function takes a file path to an email file and a string, the term
# parameter, and returns the count of that term in the email body.
count_word <- function(path, term) {
  msg <- get_msg(path)
  msg.corpus <- Corpus(VectorSource(msg))
  # Hard-coded TDM control
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE)
  msg.tdm <- TermDocumentMatrix(msg.corpus, control)
  word.freq <- rowSums(as.matrix(msg.tdm))
  term.freq <- word.freq[which(names(word.freq) == term)]
  # We use ifelse here because term.freq = NA if nothing is found
  return(ifelse(length(term.freq) > 0, term.freq, 0))
}


# With all of our support functions written, we can perform the classification.
# First, we create document corpus for spam messages

# Get all the SPAM-y email into a single vector
spam_docs <- dir(spam_path, full.names = TRUE)
all_spam <- spam_docs %>% lapply(get_msg) %>% unlist

# Create a DocumentTermMatrix from that vector
spam_tdm <- get_tdm(all_spam)

# Create a data frame that provides the feature set from the training SPAM data
spam_matrix <- as.matrix(spam_tdm)
spam_counts <- rowSums(spam_matrix)
spam_df <- as_data_frame(spam_counts) %>%
  rename(frequency = value) %>%
  rownames_to_column("term")
spam_df



spam_occurrence <- sapply(1:nrow(spam_matrix), function(i) {
  length(which(spam_matrix[i, ] > 0)) / ncol(spam_matrix)})
spam_density <- spam_df$frequency / sum(spam_df$frequency)

# Add the term density and occurrence rate
spam_df <- spam_df %>%
  mutate(density = spam_density, occurrence = spam_occurrence)

spam_df %>% arrange(desc(occurrence))




# This is the our workhorse function for classifying email.  It takes
# two required paramters: a file path to an email to classify, and
# a data frame of the trained data.  The function also takes two
# optional parameters.  First, a prior over the probability that an email
# is SPAM, which we set to 0.5 (naive), and constant value for the
# probability on words in the email that are not in our training data.
# The function returns the naive Bayes probability that the given email
# is spam_
classify_email <- function(path, training.df, prior = 0.5, c = 1e-6) {
  # Here, we use many of the support functions to get the
  # email text data in a workable format
  msg <- get_msg(path)
  msg.tdm <- get_tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  # Find intersections of words
  msg.match <- intersect(names(msg.freq), training.df$term)
  # Now, we just perform the naive Bayes calculation
  if (length(msg.match) < 1) {
    return(prior * c ^ (length(msg.freq)))
  } else {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}





# Now do the same for the EASY HAM email
easyham_docs <- dir(easyham_path)
easyham_docs <- easyham_docs[which(easyham_docs != "cmds")]
all.easyham <- sapply(easyham_docs[1:length(spam_docs)],
                      function(p) get_msg(file.path(easyham_path, p)))

easyham.tdm <- get_tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts),
                               as.numeric(easyham.counts)),
                         stringsAsFactors = FALSE)
names(easyham.df) <- c("term", "frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)
easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                            function(i)
                            {
                              length(which(easyham.matrix[i, ] > 0)) / ncol(easyham.matrix)
                            })
easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)

easyham.df <- transform(easyham.df,
                        density = easyham.density,
                        occurrence = easyham.occurrence)

# Run classifer against HARD HAM
hardham_docs <- dir(hardham_path)
hardham_docs <- hardham_docs[which(hardham_docs != "cmds")]

hardham.spamtest <- sapply(hardham_docs,
                           function(p) classify_email(file.path(hardham_path, p), training.df = spam_df))

hardham.hamtest <- sapply(hardham_docs,
                          function(p) classify_email(file.path(hardham_path, p), training.df = easyham.df))

hardham.res <- ifelse(hardham.spamtest > hardham.hamtest,
                      TRUE,
                      FALSE)
summary(hardham.res)

# Find counts of just terms 'html' and 'table' in all SPAM and EASYHAM docs, and create figure
html.spam <- sapply(spam_docs,
                    function(p) count_word(file.path(spam_path, p), "html"))
table.spam <- sapply(spam_docs,
                     function(p) count_word(file.path(spam_path, p), "table"))
spam_init <- cbind(html.spam, table.spam, "SPAM")

html.easyham <- sapply(easyham_docs,
                       function(p) count_word(file.path(easyham_path, p), "html"))
table.easyham <- sapply(easyham_docs,
                        function(p) count_word(file.path(easyham_path, p), "table"))
easyham.init <- cbind(html.easyham, table.easyham, "EASYHAM")

init.df <- data.frame(rbind(spam_init, easyham.init),
                      stringsAsFactors = FALSE)
names(init.df) <- c("html", "table", "type")
init.df$html <- as.numeric(init.df$html)
init.df$table <- as.numeric(init.df$table)
init.df$type <- as.factor(init.df$type)

init.plot1 <- ggplot(init.df, aes(x = html, y = table)) +
  geom_point(aes(shape = type)) +
  scale_shape_manual(values = c("SPAM" = 1, "EASYHAM" = 3), name = "Email Type") +
  xlab("Frequency of 'html'") +
  ylab("Freqeuncy of 'table'") +
  stat_abline(yintersept = 0, slope = 1) +
  theme_bw()
ggsave(plot = init.plot1,
       filename = file.path("./", "03-Classification", "images", "01_init_plot1.pdf"),
       width = 10,
       height = 10)

init.plot2 <- ggplot(init.df, aes(x = html, y = table)) +
  geom_point(aes(shape = type), position = "jitter") +
  scale_shape_manual(values = c("SPAM" = 1, "EASYHAM" = 3), name = "Email Type") +
  xlab("Frequency of 'html'") +
  ylab("Freqeuncy of 'table'") +
  stat_abline(yintersept = 0, slope = 1) +
  theme_bw()
ggsave(plot = init.plot2,
       filename = file.path("./", "03-Classification", "images", "02_init_plot2.pdf"),
       width = 10,
       height = 10)

# Finally, attempt to classify the HARDHAM data using the classifer developed above.
# The rule is to classify a message as SPAM if Pr(email) = SPAM > Pr(email) = HAM
spam_classifier <- function(path) {
  pr.spam <- classify_email(path, spam_df)
  pr.ham <- classify_email(path, easyham.df)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}

# Get lists of all the email messages
easyham2_docs <- dir(easyham2_path)
easyham2_docs <- easyham2_docs[which(easyham2_docs != "cmds")]

hardham2_docs <- dir(hardham2_path)
hardham2_docs <- hardham2_docs[which(hardham2_docs != "cmds")]

spam2_docs <- dir(spam2_path)
spam2_docs <- spam2_docs[which(spam2_docs != "cmds")]

# Classify them all!
easyham2_class <- suppressWarnings(lapply(easyham2_docs,
                                   function(p)
                                   {
                                     spam_classifier(file.path(easyham2_path, p))
                                   }))
hardham2_class <- suppressWarnings(lapply(hardham2_docs,
                                   function(p)
                                   {
                                     spam_classifier(file.path(hardham2_path, p))
                                   }))
spam2_class <- suppressWarnings(lapply(spam2_docs,
                                function(p)
                                {
                                  spam_classifier(file.path(spam2_path, p))
                                }))

# Create a single, final, data frame with all of the classification data in it
easyham2_matrix <- do.call(rbind, easyham2_class)
easyham2_final <- cbind(easyham2_matrix, "EASYHAM")

hardham2_matrix <- do.call(rbind, hardham2_class)
hardham2_final <- cbind(hardham2_matrix, "HARDHAM")

spam2_matrix <- do.call(rbind, spam2_class)
spam2_final <- cbind(spam2_matrix, "SPAM")

class.matrix <- rbind(easyham2_final, hardham2_final, spam2_final)
class_df <- data.frame(class.matrix, stringsAsFactors = FALSE)
names(class_df) <- c("Pr.SPAM" ,"Pr.HAM", "Class", "Type")
class_df$Pr.SPAM <- as.numeric(class_df$Pr.SPAM)
class_df$Pr.HAM <- as.numeric(class_df$Pr.HAM)
class_df$Class <- as.logical(as.numeric(class_df$Class))
class_df$Type <- as.factor(class_df$Type)

# Create final plot of results
class.plot <- ggplot(class_df, aes(x = log(Pr.HAM), log(Pr.SPAM))) +
    geom_point(aes(shape = Type, alpha = 0.5)) +
    stat_abline(yintercept = 0, slope = 1) +
    scale_shape_manual(values = c("EASYHAM" = 1,
                                  "HARDHAM" = 2,
                                  "SPAM" = 3),
                       name = "Email Type") +
    scale_alpha(guide = "none") +
    xlab("log[Pr(HAM)]") +
    ylab("log[Pr(SPAM)]") +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())
ggsave(plot = class.plot,
       filename = file.path("./", "03-Classification", "images", "03_final_classification.pdf"),
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
write.csv(easyham.df, file.path("./", "03-Classification", "data", "easyham_df.csv"), row.names = FALSE)
