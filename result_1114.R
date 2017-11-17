LexisNexis_Orlando <- read_LexisNexis("LexisNexis_v1.db", metadata = TRUE, format = TRUE)
document <- LexisNexis_Orlando$document
meta <- LexisNexis_Orlando$meta

text_stem <- create_DTM(document, ID, FULL_TEXT, n_gram = 1, stemming = TRUE)
text <- create_DTM(document, ID, FULL_TEXT, n_gram = 1, stemming = FALSE)

freq_plot <- plot_word_freq(text, q=0.1, display = TRUE)

stm_affin <- extract_sentiment(text, "afinn")
stm_nrc <- extract_sentiment(text, "nrc")

lda_result <- run_LDA(text_stem, topic_num = 6, topic_term_n = 20, q = 0.1)
Beta <- lda_result$Beta
Gamma <- lda_result$Gamma %>% rename(ID = document)
print(lda_result$topic_term_plot)

merged_stm <- merge_data(main_df = document %>% select(ID, LENGTH, DOCUMENT_DATE),
                     list_df = list(stm_affin),
                     ID = "ID")

plot_ts(merged_stm, DOCUMENT_DATE, score, stat="mean", ylab="sentiment score")

merged_topic <- merge_data(main_df = Gamma,
                           list_df = list(select(document, ID, LENGTH, DOCUMENT_DATE)),
                           ID = "ID")
plot_ts(merged_topic, DOCUMENT_DATE, gamma, color = topic, stat="mean", ylab="Gamma")
