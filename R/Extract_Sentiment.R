#' @title Extract sentiment vectors from a document-term data_frame
#' @description Input the document-term data_frame, match each word in each document
#' to a sentiment database, and calculate the sentiment intensity of each document
#' @author Jiacheng He
#'
#' @param df The document-term data_frame output from create_DTM
#' @param lexicon The sentiment database. lexicon = c("affin", "bing", "nrc", "loughran")
#'
#' @return A data_frame where each row represents a document and each column represents a sentiment.
#' If we use "affin" then it returns the sentiment score
#' @examples
#' extract_sentiment(text, "nrc")
#' extract_sentiment(text, "affin")


extract_sentiment <- function(df, lexicon) {

  library(tidytext); library(tidyverse)

  sentiment_lib <- get_sentiments(lexicon)

  if (lexicon == "afinn") {
    sentiment_scores <- left_join(df, sentiment_lib, by = "word") %>%
      replace_na(list(score = 0)) %>%
      group_by(ID) %>%
      summarize(score = sum(score))
    return(sentiment_scores)

  }  else {
    sentiment_vectors <- left_join(df, sentiment_lib, by = "word") %>%
      filter(!is.na(sentiment)) %>%
      group_by(ID, sentiment) %>%
      summarize(n = sum(n)) %>%
      spread(sentiment, n)

    sentiment_vectors[is.na(sentiment_vectors)] = 0
    return(sentiment_vectors)
  }
}
