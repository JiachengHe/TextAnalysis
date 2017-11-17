#' @title Select documents containing a certain set of key words
#' @description Input the document-term data_frame and specify the key words
#' @author Jiacheng He
#'
#' @param df The document-term data_frame output from create_DTM
#' @param key_words A list of key words
#'
#' @return A subset of the input document-term data frame with only the documents containing the key words
#' @examples
#' search_key_words(text, list("Trump"))
#' search_key_words(text, list("lgbt", "gunman", "killer"))


search_key_words <- function(df, key_words) {

  library(stringr); library(tidytext); library(tidyverse)

  search_key_word <- function(key_word, df) {

    key_word <- str_to_lower(key_word)
    ID_with_keyword <- filter(df, word == key_word) %>%
      select(ID) %>%
      unique()
  }

  ID_with_keywords <- lapply(key_words, search_key_word, df) %>%
    unlist() %>%
    unique()

  return(filter(df, ID %in% ID_with_keywords))

}
