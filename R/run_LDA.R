#' @title Run LDA
#' @description Run LDA
#' @author Jiacheng He
#' 
#' @param df The document-term data_frame output from create_DTM
#' @param topic_num An integer specifying the number of topics
#' @param seed_num An integer specifying the random seed in the algorithm
#' @param topic_term_n An integer sepcifying how many most frequent words within each topic to display. If default to NULL, then don't plot
#' @param q A numeric specifying the quantile of tf-idf to remove words. If default to NULL, then don't remove
#' 
#' @return A list of the beat matrix, the gamma matrix, and the topic-term histogram
#' @examples 
#' run_LDA(text, 6)
#' 
#' lda_output <- run_LDA(text, 6, seed_num = 731, topic_term_n = 20)
#' print(lda_output$topic_term_plot)


run_LDA <- function(df, topic_num, seed_num=731, topic_term_n=NULL, q=NULL) {
  
  library(tidyverse); library(tidytext); library(topicmodels)
  
  if (!is.null(q)) {df <- filter(df, tf_idf > quantile(tf_idf, q))}
  
  lda_result <- df %>% 
    cast_dtm(ID, word, n) %>% 
    LDA(k = topic_num, control = list(seed = seed_num))
  
  Beta <- tidy(lda_result, matrix = "beta")
  Gamma <- tidy(lda_result, matrix = "gamma")
  
  if (!is.null(topic_term_n)) {
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
    return(list(Beta = Beta, Gamma = Gamma, topic_term_plot = topic_term_plot))
  
  } else {
    return(list(Beta = Beta, Gamma = Gamma))
  }
  
}
