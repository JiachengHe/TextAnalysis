# TextAnalysis

This is an R package documents the workflow of text mining, topic modeling, and sentiment analysis. Specifically, it was used for the project to analyze Twitter and news articles related to the Orlando Shooting incidence. At the time we work on this project, another shooting incidence occurred in Las Vegas. We also compared the response to Orlando Shooting v.s. Las Vegas Shooting.



This is version_1.

There are bugs. Will fix them.

### Loading data

We store the LexisNexis news data into the SQLite database.
```R
LexisNexis_Orlando <- read_LexisNexis("LexisNexis_v1.db", metadata=TRUE, format=TRUE)
document <- LexisNexis_Orlando$document
meta <- LexisNexis_Orlando$meta
```

### Document-term matrix

The ```create_DTM``` function can create the document term matrix conveniently.
```R
text_stemmed <- create_DTM(document, ID=ID, text=FULL_TEXT, n_gram=1, stemming=TRUE)
text_nonstemmed <- create_DTM(document, ID=ID, text=FULL_TEXT, n_gram=1, stemming=FALSE)
```

### Visualize the top 10% frequent words

The nonstemmed version of text
```R
freq_plot <- plot_word_freq(text_nonstemmed, q=0.1, display = TRUE)
```

### Sentiment analysis

We can use ```extract_sentiment``` function to calculate the sentiment scores with different lexicons, for example, ```afinn``` and ```nrc```.
```R
stm_affin <- extract_sentiment(text, "afinn")
stm_nrc <- extract_sentiment(text, "nrc")
```

### LDA topics

```run_LDA``` wraps the results from Latent Dirichlet Allocation (LDA) model.
```R
lda_result <- run_LDA(text_stem, topic_num = 6, topic_term_n = 20, q = 0.1)
Beta <- lda_result$Beta
Gamma <- lda_result$Gamma %>% rename(ID = document)
print(lda_result$topic_term_plot)
```
