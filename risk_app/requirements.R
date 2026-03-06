p <- c("tidyquant", "plotly", "TTR", "tidyverse",
       "dplyr", "magrittr", "lubridate", "tibble",
       "RTL", "splines", "shiny", "bslib", "ggcorrplot", "factoextra", "gt", "Rcpp","arrow")
new.packages <- p[!(p %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE)
}
