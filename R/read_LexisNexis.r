#' @title Read LexisNexis data from the database
#' @description Throw in the database and return many data frames
#' @author Jiacheng He
#'
#' @param db The name of the LexisNexis database
#' @param metadata A logical specifying whether to read the metadata. Default to FALSE
#' @param format A logical. If TRUE, then organize the metadata (make it nice to merge).
#'
#' @return A document data_frame; or a list of document data_frame and metadata
#' @examples
#' document <- read_LexisNexis("LexisNexis_v1.db", metadata = FALSE)
#'
#' LN_Orlando <- read_LexisNexis("LexisNexis_v1.db", metadata = TRUE)
#' document <- LN_Orlando$document
#' state <- LN_Orlando$metadata$state

read_LexisNexis <- function(db, metadata=FALSE, format=FALSE) {

  library(RSQLite); library(lubridate); library(tidyverse)

  sqlite_driver <- dbDriver("SQLite")
  db <- dbConnect(sqlite_driver, dbname = db)
  dbListTables(db)

  document <- dbReadTable(db, "DOCUMENT")
  ### Formatting the "dates"
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  document$LOAD_DATE <- as.Date(document$LOAD_DATE, format = "%B %d, %Y ")
  document$DOCUMENT_DATE <- as.Date(document$DOCUMENT_DATE, format = "%B %d, %Y ")
  document$DOCUMENT_WEEK <- paste(week(ymd(document$DOCUMENT_DATE)), year(document$DOCUMENT_DATE))
  document$DOCUMENT_MONTH <- paste(month(ymd(document$DOCUMENT_DATE)), year(document$DOCUMENT_DATE))


  if (!metadata) {
    return(document)


  } else {

    meta <- list(city = dbReadTable(db, "CITY"),
                     company = dbReadTable(db, "COMPANY"),
                     source = dbReadTable(db, "SOURCE"),
                     state = dbReadTable(db, "STATE"),
                     industry = dbReadTable(db, "INDUSTRY"),
                     person = dbReadTable(db, "PERSON"),     ## celebrity mentioned?
                     subject = dbReadTable(db, "SUBJECT"),   ## key words?
                     organization = dbReadTable(db, "ORGANIZATION"),   ## organization mentioned?
                     country = dbReadTable(db, "COUNTRY"),
                     ticker = dbReadTable(db, "TICKER"),
                     region = dbReadTable(db, "REGION"))

    if (format) {
      meta$city <- meta$city %>%
        select(ID = DOCUMENT_ID, CITY = VALUE)

      meta$state <- meta$state %>%
        select(ID = DOCUMENT_ID, STATE = VALUE)
    }

    return(list(document = document, meta = meta))
  }

}
