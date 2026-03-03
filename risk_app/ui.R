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
    layout_sidebar(
      sidebar = sidebar(
        numericInput(inputId = "face_value", label = "Face Value ($)", value = 100000),
        numericInput(inputId = "coupon_rate", label = "Coupon Rate (%)", min = 0, max = 25, value = 4),
        numericInput(inputId = "time_to_maturity", label = "Time to Maturity (years)", min = 0.083, max = 30, value = 10),
        selectInput(inputId = "payment_frequency", label = "Coupon Payment Frequency", choices = c("Annual", "Semi-Annual", "Zero-Coupon"), selected = "Semi-Annual"),
        actionButton(inputId = "add_bond", label = "Add Bond to Portfolio"),
        open = "always"
      ),
      column(width = 12,
             DTOutput("bond_table")
      ))
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