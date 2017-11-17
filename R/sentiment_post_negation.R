#' @title Extract the sentiment from documents after considering a set of negation words
#' @description To address the ""NOT HAPPY" problem
#' @author Jiacheng He
#'
#' @param df The original data frame (LexisNexis, Twitter, etc)
#' @param ID The name of the variable identifying each document in the original data frame
#' @param text The name of the variable containing the texts we want to analyze in the original data frame
#' @param lexicon The sentiment database. lexicon = c("affin", "bing", "nrc", "loughran")
#' @param negation_words A list of character
#'
#' @return A data_frame where each row represents a document and each column represents a sentiment.
#' If we use "affin" then it returns the sentiment score
#' @examples
#' sentiment_post_negation(document, ID, FULL_TEXT, "nrc", list("no", "not"))
#'


sentiment_post_negation <- function(df, ID, text, lexicon, negation_words) {

  library(tidytext); library(tidyverse)

  stm <- create_DTM(df, ID, text, n_gram = 1) %>%
    extract_sentiment(lexicon)

  stm_negation <- create_DTM(df, ID, text, n_gram = 2, stop_rm=FALSE) %>%
    separate(word, c("word1", "word2"), sep = " ") %>%
    filter(word1 %in% unlist(negation_words)) %>%
    mutate(word = word2) %>%
    select(-word1, -word2) %>%
    extract_sentiment(lexicon)

  stm_negation <- data.frame(ID = stm$ID) %>%
    left_join(stm_negation)
  stm_negation[is.na(stm_negation)] = 0

  stm_net <- stm - 2 * stm_negation
  stm_net$ID <- stm$ID

  return(stm_net)
}
