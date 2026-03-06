# Shiny App - Interest Rate Risk

Shiny App designed to assist fixed income portfolio managers in understanding interest rate risk. Contains three tabs with general yield curve info, a portfolio selection where users can select up to six unique bonds to a portfolio and see risk analytics, and a co-dynamics tab that show correlations between the different selected maturities for any selected date range.

# Deployment

## Building the docker image (Local)

```terminal
docker build -t interest-risk-shiny
ducker run -p 3838:3838 interest risk shiny
```
Ensure you have navigated within the folder before doing this.

## Running Via Dockerhub

``` terminal
docker pull cainaidoo/interest-risk-shiny:latest
docker run -p 3838:3838 cainaidoo/interest-risk-shiny:latest
```


