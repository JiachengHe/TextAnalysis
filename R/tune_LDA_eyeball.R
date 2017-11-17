#' @title Use your eyeball to detect the optimal number of topics which deliever meaningful stuff
#' @description Input the range of number of topics then it output the topic-term histogram
#' from the training result of each number
#' @author Jiacheng He
#'
#' @param df The document-term data_frame output from create_DTM
#' @param k_range A vector of integers specifying the range of number of topics
#' @param seed_num An integer specifying the random seed in the algorithm
#' @param topic_term_n An integer sepcifying how many most frequent words within each topic to display
#' @param q A numeric specifying the quantile of tf-idf to remove words. If default to NULL, then don't remove
#'
#' @return A list of Topic-term histograms
#' @examples
#'
#' lda_output <- run_LDA(text, 6, seed_num = 731, topic_term_n = 20)
#' print(lda_output[[1]])
#' print(lda_output[[2]])



tune_LDA_eyeball <- function(df, k_range, seed_num=731, topic_term_n, q=NULL) {

  library(tidyverse); library(tidytext); library(topicmodels)


  iteration <- function(topic_num, df, seed_num=731, topic_term_n, q=NULL) {

    if (!is.null(q)) {df <- filter(df, tf_idf > quantile(tf_idf, q))}

    lda_result <- df %>%
      cast_dtm(ID, word, n) %>%
      LDA(k = topic_num, control = list(seed = seed_num))

    Beta <- tidy(lda_result, matrix = "beta")

    topic_term_plot <- Beta %>%
      group_by(topic) %>%
      top_n(topic_term_n, beta) %>%
      ungroup() %>%
      arrange(topic, -beta) %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip()

    return(topic_term_plot)
  }

  return(lapply(k_range, iteration, df, seed_num, topic_term_n, q))
}
