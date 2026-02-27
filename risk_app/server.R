#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

  output$calculator_button <- renderUI({
    actionButton("calculator_button","To Calculator")
  })
  output$info_button <- renderUI({
    actionButton("info_button","To Info")
  })

  plot_data <- reactive({
    rates_data_clean %>%
      filter(symbol == "DGS10") %>%
      filter(!is.na(price))
  })

    output$example_plot <- renderPlot({
      ggplot(plot_data(), aes(x = date, y = price)) +
        geom_line(color = "#2c7bb6", linewidth = 0.7) +
        labs(
          title = "10-Year Treasury Yield Over Time",
          x     = "Date",
          y     = "Yield (%)"
        ) +
        theme_minimal()
    })

    output$example_table <- renderTable({
      plot_data() %>%
        select(Date = date, `Yield (%)` = price) %>%
        arrange(desc(Date)) %>%
        head(20)
    })





  observeEvent(input$calculator_button, {
    nav_select("navbar1", selected = "Calculator")
  })
  observeEvent(input$info_button, {
    nav_select("navbar1", selected = "General Info")
  })
}


