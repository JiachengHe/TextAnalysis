#' @title Group the documents into date, and plot the time series
#' @description Plot the time series of sentiment, topics, etc
#' @author Jiacheng He
#'
#' @param df A merged data frame
#' @param date The name of the variable identifying the date
#' @param v The name of the variable you want to plot
#' @param w The name of the weighting variable when doing aggregation. If default to 1, then unweighted.
#' @param stat A character specifying the method to aggregate the documents into date stat = c("sum", "mean")
#' @param method A character specifying the method in ggsmooth
#' @param span An numeric specifying the smoothness. The larger the smoother. Default to 0.1
#' @param display A logical. If TURE, then print the plot
#'
#' @return A ggplot
#' @examples
#' plot_ts(merged, DOCUMENT_DATE, score, w = LENGTH, stat = "mean", span = 0.1,
#'         ylab = "sentiment score")
#'


plot_ts <- function(df, date, v, w=1, stat="sum", method="loess", span=0.1,
                    xlab="date", ylab=NULL, color=NULL, display=FALSE) {

  library(tidytext); library(tidyverse)

  date <- enquo(date); v <- enquo(v); w <- enquo(w); color <- enquo(color)

  df <- df %>% select(date = UQ(date), v = UQ(v), w = UQ(w), color = UQ(color))

  if (stat == "sum") {
    df <- df %>%
      group_by(date) %>%
      summarise(v = weighted.mean(v, w, na.rm = TRUE) * sum(w))
  }
  if (stat == "mean") {
    df <- df %>%
      group_by(date, color) %>%
      summarise(v = weighted.mean(v, w, na.rm = TRUE))
  }

  {

    ts <- df %>%
      ggplot(aes(date, v)) +
      geom_smooth(aes(color = factor(color)), method = method, span = span) +
      xlab(xlab) +
      ylab(ylab)
  }

  if (display) {print(ts)}
  return(ts)
}
