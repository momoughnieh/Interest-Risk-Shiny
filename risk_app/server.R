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
    rateData %>%
      filter(maturity == 10)
  })

    output$example_plot <- renderPlot({
      ggplot(plot_data(), aes(x = date, y = rate)) +
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
        select(Date = date, `Yield (%)` = rate) %>%
        arrange(desc(Date)) %>%
        head(20)
    })





  observeEvent(input$calculator_button, {
    nav_select("navbar1", selected = "Calculator")
  })
  observeEvent(input$info_button, {
    nav_select("navbar1", selected = "General Info")
  })
  pca_estimates <- ir.wide %>%
    recipes::recipe(~ .) %>%
    recipes::step_center(all_numeric()) %>%
    recipes::step_scale(all_numeric()) %>%
    recipes::step_pca(all_numeric(), num_comp = 3) %>%
    recipes::prep(training = ir.wide)

  output$corrPlot <- renderPlot({
    ir.wide %>%
      dplyr::select(-date) %>%
      stats::cor(method = "kendall") %>%
      ggcorrplot::ggcorrplot(
        type   = "lower",
        colors = c("green", "yellow", "blue"),
        title  = "Correlation Matrix",
        lab    = TRUE
      )
  })

  output$screePlot <- renderPlot({
    ir.wide %>%
      dplyr::select(-date) %>%
      stats::prcomp(scale = TRUE, center = TRUE) %>%
      factoextra::fviz_eig(choice = "variance", addlabels = TRUE, ncp = 11)
  })

  output$loadingsPlot <- renderPlot({
    tidy(pca_estimates, number = 3) %>%
      dplyr::filter(component %in% c("PC1", "PC2", "PC3")) %>%
      dplyr::mutate(terms = as.numeric(terms)) %>%
      ggplot(aes(x = terms, y = value, group = component, col = component)) +
      geom_line() +
      labs(title = "Key Principal Component Loadings",
           x = "Maturity (Years)", y = "Loading") +
      theme_minimal()
  })



}


