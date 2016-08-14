# Return a single element vector of just the email body. This is a very simple
# approach, as we are only using words as features.
get_msg <- function(path) {
  text <- readLines(path, encoding = "latin1")

  # The message always begins after the first full line break.
  msg_start <- which(text == "")[1] + 1

  # "The “null line” separating the header from the body of an email is part of
  # the protocol definition. For reference, see RFC822:
  # http://tools.ietf.org/html/frc822." (p. 80).

  # Return an empty vector is no line break is found
  if (!is.na(msg_start)) {
    msg_lines <- text[seq(msg_start, length(text), 1)]
    msg <- paste(msg_lines, collapse = "\n")
  } else{
    msg <- character(0)
  }

  msg
}


# Given a term-document matrix, create a data frame that provides the feature
# set from some training data.
convert_tdm_to_training_df <- function(msg_tdm) {
  term_df <- convert_tdm_to_df(msg_tdm)

  # Proportion of documents containing each term.
  msg_matrix <- as.matrix(msg_tdm)
  term_occurrence <- rowMeans(0 < msg_matrix) %>% unname

  # Add the term density and occurrence rate.
  term_df %>%
    mutate(density = frequency / sum(frequency),
           occurrence = term_occurrence)
}

# Count frequency of terms in a term-document matrix
convert_tdm_to_df <- function(msg_tdm) {
  term_counts <- msg_tdm %>%
    as.matrix %>%
    rowSums()
  term_df <- as_data_frame(term_counts) %>%
    rename(frequency = value) %>%
    rownames_to_column("term")
  term_df
}




# "One way of quantifying the frequency of terms in our spam email is to
# construct a _term document matrix_ (TDM). As the name suggests, a TDM is an
# N×M matrix in which the terms found among all of the documents in a given
# corpus define the rows and all of the documents in the corpus define the
# columns. The `[i, j]` cell of this matrix corresponds to the number of times
# term `i` was found in document `j`." (p. 81).

# These are the stopwords that are excluded.
# tm::stopwords("en")

# Create a TermDocumentMatrix (TDM) from the corpus of SPAM email. The TDM
# control can be modified, and the sparsity level can be altered. This TDM is
# used to create the feature set used to do train our classifier.
get_tdm <- function(doc_vec) {
  control <- list(
    removePunctuation = TRUE,
    removeNumbers = TRUE,
    stopwords = TRUE,
    minDocFreq = 2)
  doc_corpus <- tm::Corpus(tm::VectorSource(doc_vec))
  tm::TermDocumentMatrix(doc_corpus, control)
}


# This function takes a file path to an email file and a string, the term
# parameter, and returns the count of that term in the email body.
count_word <- function(path, search_term) {
  msg <- get_msg(path)
  msg_corpus <- tm::Corpus(tm::VectorSource(msg))

  # Hard-coded TDM control
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE)
  msg_tdm <- tm::TermDocumentMatrix(msg_corpus, control)
  results <- msg_tdm %>%
    convert_tdm_to_df %>%
    filter(term == search_term)

  word_freq <- ifelse(nrow(results) == 0, 0, results$frequency)
  word_freq
}
