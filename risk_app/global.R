library(dplyr)
library(magrittr)
library(lubridate)
library(tibble)
library(plotly)
library(RTL)
library(tidyquant)
library(dplyr)
library(splines)
library(stringr)

# Goal: Figure out how to pull data through github actions, and then read data in app.

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

getSplineCurve <- function(yieldCurve) {

  # This function takes a data frame of different single point rates at different maturities and fits a curve
  # Used to interpolate rates in between rates pulled from FRED (eg. incase we need a rate for 1.5Y maturity)

  yieldFit <- stats::lm(rate ~ splines::bs(maturity, knots = c(2,3,10), degree = 3), yieldCurve)

  return(yieldFit)
}

bootsrapSpot <- function(parFit) {
  # Bootstrap spot yields #

  # Price at par is 100

  parPrice <- 100

  # Assuming 1, 3, and 6 month par yields are equal to spot (no coupons)

  all_maturities <- data.frame(
    maturity = seq(0.5, 30, by = 0.5)
  )

  # Generate par yields for all coupon payment dates
  parRates <- parFit %>%
    stats::predict(newdata = all_maturities) %>%
    unname(.)

  parYields <- tibble(maturity = seq(0.5, 30, by = 0.5),
                      parRate = parRates)

  spotRates <- tibble(maturity = c(0.5),
                       spotRate = c(parYields$parRate[1]))

  # Back out spot rates for every maturity, add to spotYields data frame.

  for (mat in parYields$maturity[2:length(parYields$maturity)]) {

    # Par yields mean YTM = coupon rate
    couponRate <- parYields %>%
      dplyr::filter(maturity == mat) %>%
      dplyr::pull(parRate)

    # Coupon payment amount
    couponAmt = (couponRate * parPrice) / 2

    # Pull available spot rates with respective maturities to discount coupon payments
    spotrates <- spotRates %>%
      dplyr::filter(maturity < mat) %>%
      dplyr::pull(spotRate)
    maturities <- spotRates %>%
      dplyr::filter(maturity < mat) %>%
      dplyr::pull(maturity)

    # Calculate vector of discount factors for every coupon date
    discFactors <- (1 / (1 + spotrates / 2)^(2*maturities))

    sumDiscFactors <- sum(discFactors)

    # Present value of coupons
    pvCoupons <- couponAmt * sumDiscFactors

    # Back out spot rate from final payment (principal + coupon)
    # Breaking apart & rearrange formula to solve for spot rate algebraically.

    finalTermDiscFactor <- (parPrice - pvCoupons) / (parPrice + couponAmt)

    newSpotRate <- 2 * ((finalTermDiscFactor^(-1/(mat * 2))) - 1)

    # Bind to dataframe before next iteration

    tempdf <- tibble(maturity = mat,
                     spotRate = newSpotRate)

    spotRates <- rbind(spotRates, tempdf)
  }
  return(spotRates)
}

