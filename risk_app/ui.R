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
    fluidRow(
      uiOutput("calculator_button")
    )
  ),
  nav_panel(
    title = "Portfolio Selection",
    fluidRow(
      selectInput("number_of_bonds", "Select Number of Bonds in Portfolio", c(1,2,3,4,5,6)),
      uiOutput("bond_inputs")
    )
  ),
  nav_panel(
    title = "Calculator",
    card(
      card_header ("Example Table"),
      tableOutput("example_table")
    ),
    fluidRow(
      uiOutput("info_button")
    )
  ),nav_panel(
    title = "Co-Dynamics",
    plotOutput("corrPlot"),
    plotOutput("screePlot"),
    plotOutput("loadingsPlot")
  )
)


