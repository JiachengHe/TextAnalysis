## Assume you have read the "DOCUMENT" file from the LexisNexis database
install.packages(TextAnalysis)
library(TextAnalysis)
library(devtools)
library(parallel)

##### Keyword lists ######
terrorism <- list("terrorism")
gun <- list("gun", "firearm", "gunfir", "gunshot", "gunfight")
trump <- list("donald", "trump")
obama <- list("barack", "obama")
shooter <- list("omar", "mateen", "stephen")


analysis<- function(gram, is_stem, keyword_list, is_sub_set) {

  text <- create_DTM(document, ID, FULL_TEXT, stemming = is_stem, n_gram = gram)

  key_text <- search_key_words(text, keyword_list)

  plot1 <- plot_word_freq(text, freq_n = 30, q = 0.2)
  plot2 <- plot_word_freq(key_text, freq_n = 30, q = 0.2)

  if(is_stem == FALSE){
    stm <- extract_sentiment(key_text, "nrc")
    return(list(stm, plot1, plot2))
  }
  else {
    no_cores = detectCores() - 1
    if(is_sub_set) {
      tune_result <- tune_LDA(key_text, 2:15, q = 0.1, mc.cores = no_cores)
      lda_output <- run_LDA(key_text, topic_num = 6, seed_num = 731)
    }
    else {
      tune_result <- tune_LDA(text, 2:15, q = 0.1, mc.cores = no_cores)
      lda_output <- run_LDA(text, topic_num = 6, seed_num = 731)
    }
    return(list(lda_output, plot1, plot2))
  } 
  
}

document()
document <- read_LexisNexis("LexisNexis_v1.db")

#### gun control ####
senti_1 <- analysis(1, FALSE, gun, TRUE)

lda_1 <- analysis(1, TRUE, gun, TRUE)
lda_2 <- analysis(2, TRUE, gun, TRUE)
lda_3 <- analysis(3, TRUE, gun, TRUE)

#### TRUMP ####
senti_2 <- analysis(1, FALSE, trump, TRUE)

lda_4 <- analysis(1, TRUE, trump, TRUE)
lda_5 <- analysis(2, TRUE, trump, TRUE)
lda_6 <- analysis(3, TRUE, trump, TRUE)

####### do same for other key topics ########

