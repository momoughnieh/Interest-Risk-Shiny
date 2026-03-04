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
        uiOutput(outputId = "coupon_rate_button"),
        numericInput(inputId = "time_to_maturity", label = "Time to Maturity (years)", min = 0.083, max = 30, value = 10),
        selectInput(inputId = "payment_frequency", label = "Coupon Payment Frequency", choices = c("Annual", "Semi-Annual", "Zero-Coupon"), selected = "Semi-Annual"),
        actionButton(inputId = "add_bond", label = "Add Bond to Portfolio"),
        actionButton(inputId = "clear_portfolio", label = "Clear Portfolio"),
        uiOutput(outputId = "clear_selected_ui"),
        actionButton(inputId = "submit_portfolio", label = "Submit Portfolio", class = "btn-success"),
        open = "always"
      ),
      column(width = 12,
             h4("Portfolio Assets (max. 6)"),
             DTOutput("bond_table")
      ),
      fluidRow(
        column(width = 1),
        column(width = 10,
               hr(),
               gt_output(outputId = "exposure_table")),
        column(width = 1)
      ),
      fluidRow(
        hr(),
        column(width = 6,
               gt_output(outputId = "pnl_by_rate")),
        column(width = 6,
               gt_output(outputId = "pnl_by_instrument"))
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