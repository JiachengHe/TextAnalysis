#' @title Find the optimal number of topics
#' @description Tune a specified range of numbers, find the optimal number of topics
#' based on some criterion measures ("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014")
#' @author Jiacheng He
#'
#' @param df The document-term data_frame output from create_DTM
#' @param k_range A vector of integers specifying the range of number of topics
#' @param seed_num An integer specifying the random seed in the algorithm
#' @param mc.cores An integer specifying how many cores to use for computing. Default to 4
#' @param verbose A logical specifying whether to display the progress when the algorith is running. Default to TRUE
#' @param q A numeric specifying the quantile of tf-idf to remove words. If default to NULL, then don't remove
#'
#' @return A list of matrix reporting the criterion measure for each number,
#' the optimal number based on Griffiths2004,
#' the optimal number based on CaoJuan2009,
#' the optimal number based on Arun2010,
#' the optimal number based on Deveaud2014,
#' and the plot of the measure matrix
#' @examples
#' tune_result <- tune_LDA(text, k_range = 2:15, q = 0.1)
#' print(tune_result$k_plot)

tune_LDA <- function(df, k_range, seed_num = 731, mc.cores = 4L, verbose = TRUE, q=NULL) {

  library(tidytext); library(tidyverse);
  library(topicmodels); library(ldatuning)

  if (!is.null(q)) {df <- filter(df, tf_idf > quantile(tf_idf, q))}

  result <- df %>%
    cast_dtm(ID, word, n) %>%
    FindTopicsNumber(topics = seq(from = 2, to = 15, by = 1),
                     metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                     method = "Gibbs",
                     control = list(seed = 731),
                     mc.cores = mc.cores,
                     verbose = verbose)

  FindTopicsNumber_plot(result)
  k_plot <- recordPlot()
  graphics.off()

  return(list(result = result,
              k_Griffiths2004 = result$topics[which.max(result$Griffiths2004)],
              k_CaoJuan2009 = result$topics[which.min(result$CaoJuan2009)],
              k_Arun2010 = result$topics[which.min(result$Arun2010)],
              k_Deveaud2014 = result$topics[which.max(result$Deveaud2014)],
              k_plot = k_plot))
}
