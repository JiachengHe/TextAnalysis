#' @title Create a tidy document-term data_frame
#' @description  Throw in the original data frame containing text, extract the text,
#' then create a document-term data_frame.
#' @author Jiacheng He
#'
#' @param df The original data frame (LexisNexis, Twitter, etc)
#' @param ID The name of the variable identifying each document in the original data frame
#' @param text The name of the variable containing the texts we want to analyze in the original data frame
#' @param n_gram A numeric specifying the the number of grams. If default to NULL, then use 1-gram
#' @param stemming A logical specifying whether to implement stemming. Default to FALSE
#' @param q A numeric specifying the quantile of tf-idf to remove words. If default to NULL, then don't remove
#'
#' @return A tidy document-term data_frame containing ID, word, word counts, and tf-idf
#' @examples
#' create_DTM(document, ID, FULL_TEXT)
#' create_DTM(document, ID, FULL_TEXT, 1, TRUE, 0.2)

create_DTM <- function(df, ID, text, n_gram=1, stop_rm=TRUE, stemming=FALSE, q=NULL) {

  library(tidytext); library(tidyverse)

  data(stop_words)

  ID = enquo(ID); text = enquo(text)

  DTM_tidy <- select(df, ID = UQ(ID), text = UQ(text))

  if (n_gram == 1) {
    DTM_tidy <- DTM_tidy %>%
      unnest_tokens(word, text)
    if (stop_rm) {
      DTM_tidy <- DTM_tidy %>%
        anti_join(stop_words)
    }
    if (stemming) {
      library(SnowballC)
      DTM_tidy <- DTM_tidy %>%
        mutate(word = wordStem(word))
    }


  } else {
    n_words <- paste0("word", 1:n_gram)
    DTM_tidy <- DTM_tidy %>%
      unnest_tokens(word, text, token = "ngrams", n = n_gram)
    if (stop_rm) {
      DTM_tidy <- DTM_tidy %>%
        separate(word, n_words, sep = " ")
      for (word_n in n_words) {
        filter_cond <- paste("!", word_n) %>%
          paste("%in% stop_words$word")
        DTM_tidy <- DTM_tidy %>%
          filter_(filter_cond)
      }
      DTM_tidy <- unite_(DTM_tidy, "word", n_words, sep = " ")
    }
  }


  DTM_tidy <- DTM_tidy %>%
    count(ID, word) %>%
    ungroup() %>%
    arrange(ID) %>%
    bind_tf_idf(word, ID, n)

  if (!is.null(q)) {
    DTM_tidy <- DTM_tidy %>%
      filter(tf_idf > quantile(tf_idf, q))
  }

  return(DTM_tidy)
}
