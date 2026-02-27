library(dplyr)
library(magrittr)
library(lubridate)
library(tibble)
library(plotly)
library(RTL)
library(tidyquant)
library(dplyr)

grabRates <- function() {
  rateTickers <- c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS25", "DGS30")
  
  # Pull data
  rateDat <- rateTickers %>%
    tq_get(get = "economic.data",
           from = "1992-01-01",
           to = Sys.Date())
  
  # Clean data
  
  rateDat <- rateDat %>%
    tidyr::drop_na() %>%
    dplyr::mutate(price = price / 100) %>%
    dplyr::rename("rate" = price) %>%
    dplyr::mutate(maturityChar = str_extract(symbol, "\\d+.*")) %>%
    dplyr::mutate(maturity = ifelse(str_detect(maturityChar, "MO"), as.numeric(str_extract(maturityChar, "\\d")) / 12, as.numeric(maturityChar))) %>%
    dplyr::select(-maturityChar, -symbol)
  
  return(rateDat)
}

rateData <- grabRates()