#' @title Merge the document features, sentiment and topic output, and meta data
#' @description Merge many features of documents, to output an integrated data_frame
#' @author Jiacheng He
#'
#' @param main_df The main data frame (ideally contains all key values)
#' @param list_df A list of data frames
#' @param ID A character, the key to be merged
#'
#' @return An integrated data frame
#' @examples
#' merged <- merge_data(main_df = document %>% select(ID, LENGTH, DOCUMENT_DATE),
#'                      list_df = list(stm, meta$city, meta$state),
#'                      ID = "ID")


merge_data <- function(main_df, list_df, ID="ID") {

  library(tidyverse)

  for (i in 1:length(list_df)) {
    main_df <- main_df %>%
      left_join(list_df[[i]], by = ID)
  }

  return(main_df)
}









