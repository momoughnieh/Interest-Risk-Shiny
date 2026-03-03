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
  
  portfolio <- reactiveVal(
    tibble(
      "bond_id" = numeric(),
      "face_value" = numeric(),
      "coupon" = numeric(),
      "maturity" = numeric(),
      "freq" = numeric(),
      "pv" = numeric()
      # "spotCurve" = list()
    )
  )
  
  addBond <- reactive({
    entirePortfolio <- portfolio()
    req(nrow(entirePortfolio) < 6)
    
    faceValue <- input$face_value
    couponRate <- input$coupon_rate
    maturity <- input$time_to_maturity
    frequency <- input$payment_frequency
    
    freqInput <- ifelse(frequency == "Semi-Annual", 2, ifelse(frequency == "Zero-Coupon", 0, 1))
    
    # lastDate <- rateData %>%
    #   tail(., n = 1L) %>%
    #   dplyr::pull(date)
    # 
    # parCurve <- rateData %>%
    #   dplyr::filter(date == lastDate)
    
    # fitPar <- getSplineCurve(parCurve)
    # spotsKey <- bootstrapSpot(fitPar, parCurve)
    
    spots <- getSplineSpot(maturity, freqInput, spotsKey)
    
    bondPrice <- priceBond(faceValue, (couponRate / 100), maturity, freqInput, spots)
    
    bond_ids <- entirePortfolio$bond_id
    
    bond_id <- ifelse(length(bond_ids) != 0, bond_ids[length(bond_ids)] + 1, 1)
    
    tempTibble <- tibble(
      "face_value" = faceValue,
      "bond_id" = bond_id,
      "coupon" = couponRate,
      "maturity" = maturity,
      "freq" = frequency,
      "pv" = bondPrice,
    ) %>%
      dplyr::mutate(freq = case_when(
        freq == "Annual" ~ 1,
        freq == "Semi-Annual" ~ 2,
        freq == "Zero-Coupon" ~ 0
      ))
    
    entirePortfolio <- bind_rows(entirePortfolio, tempTibble)
    
    return(entirePortfolio)
    
  }) %>%
    bindEvent(input$add_bond)
  
  observeEvent(input$add_bond, {
    portfolio(addBond())
  })
  
  ## Remove selected bonds
  
  removeBonds <- reactive({
    entirePortfolio <- portfolio()
    
    rowsRemove <- input$bond_table_rows_selected
    
    entirePortfolio <- entirePortfolio %>%
      dplyr::filter(!bond_id %in% rowsRemove)
  }) %>%
    bindEvent(input$clear_selected_bonds)
  
  
  observeEvent(input$clear_selected_bonds, {
    portfolio(removeBonds())
  })
  
  ##
  
  ## Remove all bonds
  
  removeAllBonds <- reactive({
    entirePortfolio <- portfolio()
    
    entirePortfolio <- tibble("bond_id" = numeric(),
                              "face_value" = numeric(),
                              "coupon" = numeric(),
                              "maturity" = numeric(),
                              "freq" = numeric(),
                              "pv" = numeric())
  }) %>%
    bindEvent(input$clear_portfolio)
  
  observeEvent(input$clear_portfolio, {
    portfolio(removeAllBonds())
  })
  
  ##
  
  output$bond_table <- renderDT({
    portfolioTable <- portfolio() %>%
      dplyr::mutate(freq = case_when(
        freq == 1 ~ "Annual",
        freq == 2 ~ "Semi-Annual",
        freq == 0 ~ "Zero-Coupon"
      )) %>%
      dplyr::select(-bond_id)
    
    DT::datatable(
      portfolioTable,
      caption = "Portfolio Composition",
      rownames = FALSE,
      colnames = c("Face Value ($)", "Coupon Rate (%)", "Time to Maturity (years)", "Payment Frequency", "Bond Value ($)"),
      width = "150px",
      selection = "multiple", 
      options = list(
        dom = "t",
        scrollY = "200px",
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
        
      )
    )
  })
  
  observe({
    print(names(input))
    print(input$bond_table_rows_selected)
  })
  
  output$clear_selected_ui <- renderUI({
    req(input$bond_table_rows_selected > 0)
    
    actionButton(inputId = "clear_selected_bonds", label = "Clear Selected Bond(s)")
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