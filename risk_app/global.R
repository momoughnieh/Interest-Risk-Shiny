library(dplyr)
library(magrittr)
library(lubridate)
library(tibble)
library(plotly)
library(RTL)
library(tidyquant)
library(dplyr)

rate_symbols <- c(
  "DGS1MO","DGS3MO","DGS6MO","DGS1","DGS2","DGS3","DGS5","DGS7","DGS10","DGS20","DGS30"
)
rates_data <- tq_get(rate_symbols, get  = "economic.data", from = "1992-01-01"
)
rates_data_clean <- rates_data %>%
  arrange(symbol, date)
