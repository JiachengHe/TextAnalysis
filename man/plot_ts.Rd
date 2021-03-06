% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ts.R
\name{plot_ts}
\alias{plot_ts}
\title{Group the documents into date, and plot the time series}
\usage{
plot_ts(df, date, v, w = 1, stat = "sum", method = "loess", span = 0.1,
  xlab = "date", ylab = NULL, color = NULL, display = FALSE)
}
\arguments{
\item{df}{A merged data frame}

\item{date}{The name of the variable identifying the date}

\item{v}{The name of the variable you want to plot}

\item{w}{The name of the weighting variable when doing aggregation. If default to 1, then unweighted.}

\item{stat}{A character specifying the method to aggregate the documents into date stat = c("sum", "mean")}

\item{method}{A character specifying the method in ggsmooth}

\item{span}{An numeric specifying the smoothness. The larger the smoother. Default to 0.1}

\item{display}{A logical. If TURE, then print the plot}
}
\value{
A ggplot
}
\description{
Plot the time series of sentiment, topics, etc
}
\examples{
plot_ts(merged, DOCUMENT_DATE, score, w = LENGTH, stat = "mean", span = 0.1,
        ylab = "sentiment score")

}
\author{
Jiacheng He
}
