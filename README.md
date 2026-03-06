# Shiny App - Interest Rate Risk

Shiny App designed to assist fixed income portfolio managers in understanding interest rate risk. 

Contains three navigation tabs which include:

### General Info

The user is able to view the historical par yields for each maturity, the shape of the par yield curve today, and the historical risk of each yield.
On the sidebar, the user can select the parameters to plot a line graph showcasing rolling volatility for the selected yield.

### Portfolio PnL & Sensitivities

On the sidebar, the user can input various bond characteristics and then add to their portfolio, which is displayed in a table on the right. The user is allowed 6 different instruments in their portfolio.

When the user submits the portfolio, a pie-chart showcasing the shares of each position based on relative value to the portfolio is displayed.

A table showcasing delta exposures for each key rate is shown. This table does not show the sensitivity to every single rate used in the pricing function, as it would be too large.

A table showcasing actual vs. attributed PnL based on previous day sensitivities. The attributed PnL is calculated based on a full table of sensitivities for each maturity rate.

Pricing is based on zero-rates which were boostrapped from the par yield curve.

### Co-dynamics

This tab showcases two spread plots which include so that the user can observe the relationship between yields historically to date.

On the sidebar, the user can select the parameters to generate a correlation matrix across various maturities. The user also has the option to select
a time period on which they want the correlations to be calculated on.

# Deployment

### Building the docker image (Local)

```terminal
docker build -t interest-risk-shiny
ducker run -p 3838:3838 interest risk shiny
```
Ensure you have navigated to the correct directory before doing this.

### Running Via Dockerhub

``` terminal
docker pull cainaidoo/interest-risk-shiny:latest
docker run -p 3838:3838 cainaidoo/interest-risk-shiny:latest
```

To view the app, type localhost:3838 in a browser. Ensure that you have Docker Desktop running in the background to run via DockerHub.


