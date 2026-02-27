#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

page_navbar(
  title = "Portfolio Risk Calculator",
  id = "navbar1",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    selectInput("coupon_rate_pick", "Select Coupoon Rate", "coupon_rate_data"),
    numericInput("time_to_maturity_input", "Select Time to Maturity (days)", value = 0, min = 0),
    selectInput("payment_frequency_pick", "Select Payment Frequency", c("Semi-Annual", "Annual", "Quarterly", "Zero-Coupon"))
    ),
  nav_panel(
    title = "General Info",
    card(
      card_header("Example Graph"),
      plotOutput("example_plot")
    ),
    card(
      uiOutput("calculator_button")
    )
  ),
  nav_panel(
    title = "Calculator",
    card(
      card_header ("Example Table"),
      tableOutput("example_table")
    ),
    card(
      uiOutput("info_button")
    )
  )
  )



