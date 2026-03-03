library(dplyr)
library(magrittr)
library(lubridate)
library(tibble)
library(plotly)
library(RTL)
library(tidyquant)
library(dplyr)
library(splines)
library(ggcorrplot)
library(factoextra)
library(recipes)
library(tidyverse)
library(DT)

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

## Spline Curve function -- to be used on par yields to bootstrap zero curve ##

getSplineCurve <- function(yieldCurve) {

  # This function takes a data frame of different single point rates at different maturities and fits a curve
  # Used to interpolate rates in between rates pulled from FRED (eg. incase we need a rate for 1.5Y maturity)

  yieldFit <- stats::lm(rate ~ splines::bs(maturity, knots = c(2,3,10), degree = 3))

  return(yieldFit)
}

## Get spline for spot ##
## This function is used to interpolate spot rates specific to a particular bond ##
## Purpose: Spline once so that we can adequately observe shock effects on particular yields. Shocking & re-splining changes rates we don't intend to shock ##

getsplineSpot <- function(mat, freq, spots) {
  
  splineFit <- stats::lm(rate ~ splines::bs(maturity, knots = c(2,3,10), degree = 3), spots)
  
  maturities <- data.frame(
    maturity = seq(1 / freq, mat, by = 1 / freq))
    
  rates <- splineFit %>% stats::predict(
    newdata = maturities
  ) %>%
    unname(.)
    
  spotCurve <- tibble(maturity = maturities$maturity,
                      rate = rates)
  
  return(spotCurve) # This is to be fed into the bond pricing function
  
}

## Function to price specific bonds ##
## Inputs: face value, coupon rate, maturity, payment frequency, and the appropriate spot rates given the payment periods ##

priceBond <- function(faceVal, C, mat, freq, spotRates) {
  
  pricingTable <- tibble(maturity = spotRates$maturity[-length(spotRates$maturity)],
                         cashFlow = ((C * faceVal) / freq),
                         rate = spotRates$rate[-length(spotRates$rate)])
  
  lastRowCF <- tibble(maturity = spotRates$maturity[lnegth(spotRates$maturity)],
                      cashFlow = faceVal + ((C * faceVal) / freq),
                      rate = spotRates$rate[length(spotRates$rate)])
  
  pricingTable <- bind_rows(pricingTable, lastRowCF) %>%
    dplyr::mutate(discFactor = (1 / (1 + (rate / freq))^(maturity*freq))) %>%
    dplyr::mutate(cashFlowPV = cashFlow * discFactor)
  
  finalPrice <- sum(pricingTable$cashFlowPV)
  
  return(finalPrice)
}

# This function comptues the sensitivites WITHIN a specific bond to each key spot rate

computeKeySens <- function(spotCurve, bondInfo) {
  
  stepSize = 0.0001 # 1 BP sensitivity
  
  faceVal <- bondInfo[1]
  couponRate <- bondInfo[2]
  maturity <- bondInfo[3]
  frequency <- bondInfo[4]
  
  priceUps <- c() 
  priceDowns <- c()
  
  for (row in 1:nrow(spotCurve)) {
    
    shockedUp <- spotCurve
    shockedUp[row, 2] <- shockedUp[row, 2] + stepSize
    
    shockedDown <- spotCurve
    shockedDown[row, 2] <- shockedDown[row, 2] - stepSize
    
    priceUp <- priceBond(faceVal, couponRate, maturity, frequency, shockedUp)
    priceDown <- priceBond(faceVal, couponRate, maturity, frequency, shockedDown)
  }
  
  # So far: Only delta calculations. Will amend later for gamma.
  
  sensTibble <- spotCurve %>%
    dplyr::select(-rate) %>%
    dplyr::mutate(priceUp = priceUps,
                  pricedown = priceDowns) %>%
    dplyr::mutate(delta = ((priceUp - priceDown) / (2 * stepSize)) / 10000) %>%
    dplyr::select(-c(priceUp, priceDown))
  
  return(sensTibble) 
  
}






bootsrapSpot <- function(parFit, yieldCurve) {
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
  
  shortTermRates <- yieldCurve %>%
    dplyr::filter(maturity < 0.5) %>%
    dplyr::rename(spotRate = rate) %>%
    dplyr::select(-date)
  
  spotRates <- rbind(spotRates, shortTermRates) %>%
    arrange(maturity)
  
  uniqueMaturities <- yieldCurve %>%
    dplyr::pull(maturity)
  
  spotRates <- spotRates %>%
    dplyr::filter(maturity %in% uniqueMaturities)
  
  return(spotRates)
}

# Function to compute key rate deltas (bond-by-bond basis)
# Parameters: spotCurve, vector of (coupon, T2M, freq)



# Step 1：ret = price - lag(price)
ir.long <- rateData %>%
  dplyr::group_by(maturity) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(ret = rate - dplyr::lag(rate)) %>%   # 课本: ret = price - lag(price)
  stats::na.omit() %>%
  dplyr::ungroup()

# Step 2:pivot_wider
ir.wide <- ir.long %>%
  tidyr::pivot_wider(
    id_cols    = date,
    names_from = maturity,
    values_from = ret
  ) %>%
  stats::na.omit()

