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
library(plotly)

page_navbar(
  title = "Interst Rate Risk App",
  id = "navbar1",
  theme = bs_theme(bootswatch = "flatly"),
  nav_panel(
    title = "General Info",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId  = "vol_maturity",
          label    = "Maturity (Years)",
          choices  = c("0.0833", "0.25", "0.5", "1", "2", "5", "7", "10", "20", "30"),
          selected = "10"
        ),
        sliderInput(
          inputId = "vol_window",
          label   = "Rolling Window (Days)",
          min = 21, max = 252, value = 63, step = 21
        ),
        actionButton(inputId = "submit_cor", label = "Submit Selection", class = "btn-success"),
        open = "always"
      ),
      column(width = 12,
             h4("U.S. Treasury Yield Curve Dynamics")
      ),
      fluidRow(
        plotlyOutput("yield_curve_dynamics", height = "400px"),
        hr(),
        h4("Today's Par Yield Curve")
      ),
      fluidRow(
        plotlyOutput("todays_curve", height = "400px"),
        hr(),
        h4("Historical Risk Overview")
      ),
      fluidRow(
        plotlyOutput("riskPlot", height = "400px"),
        hr(),
        h4("Rolling Volatility Plot")
      ),
      fluidRow(
        plotlyOutput("rolling_vol_plot", height = "400px")
      )
    )
  ),
  nav_panel(
    title = "Portfolio Sensitivity & PnL",
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
               plotlyOutput("portfolio_pie", height = "350px")),
        column(width = 1)
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
      ),
    ),
  ),
  nav_panel(
    title = "Co-Dynamics",
    layout_sidebar(
      sidebar = sidebar(
        h4("Select Correlation Matrix Parameters"),
        checkboxGroupInput(
          inputId  = "corr_selection",
          label    = "Maturity (Years)",
          choices  = c("0.0833", "0.25", "0.5", "1", "2", "5", "7", "10", "20", "30"),
          selected = "10"
        ),
        dateRangeInput(
          inputId = "corr_date_selection",
          label = "Select Date Range",
          start = "1992-01-01",
          end = Sys.Date(),
          min = "1992-01-01",
          max = Sys.Date()
        ),
        actionButton(inputId = "submit_corr", label = "Submit Selection", class = "btn-success"),
        open = "always"
      ),
      column(width = 12,
             h4("Co-Dynamics")
      ),
      fluidRow(
        h4("Yield Correlation Matrix"),
        gt_output("corr_table"),
        hr()
      ),
      fluidRow(
        h4("Par Yield Spreads"),
        plotlyOutput("spread_plot")
      ),
      fluidRow(
        plotlyOutput("spread_plot_butterfly")
      )
    )
  )
)
