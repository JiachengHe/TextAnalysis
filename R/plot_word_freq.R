#' @title Plot the frequency of the most frequent words
#' @description Input the document-term data_frame, then plot the the frequency of the most frequent words with histogram
#' @author Jiacheng He
#'
#' @param df The document-term data_frame output from create_DTM
#' @param freq_n A numeric specifying how many most frequent words we want to display. Defalut to 30.
#' @param q A numeric specifying the quantile of tf-idf to remove words. If default to NULL, then don't remove
#'
#' @return A ggplot histogram
#' @examples
#' plot_word_freq(text, freq_n = 30, q = 0.2)
#'

plot_word_freq <- function(df, freq_n=30, q=NULL, display=FALSE) {

  library(tidyverse); library(tidytext)

  if (!is.null(q)) {df <- filter(df, tf_idf > quantile(tf_idf, q))}

  word_freq_plot <- df %>%
    group_by(word) %>%
    summarise(n = sum(n)) %>%
    mutate(word = reorder(word, n)) %>%
    top_n(freq_n, n) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

  if (display) {print(word_freq_plot)}
  return(word_freq_plot)
}
