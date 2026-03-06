#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


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
      "pv" = numeric(),
      "pv_yest" = numeric(),
      "keySens" = list(),
      "keySensYesterday" = list()
    )
  )

  addBond <- reactive({
    entirePortfolio <- portfolio()
    req(nrow(entirePortfolio) < 6)

    faceValue <- input$face_value
    maturity <- input$time_to_maturity
    frequency <- input$payment_frequency
    freqInput <- ifelse(frequency == "Semi-Annual", 2, ifelse(frequency == "Zero-Coupon", 0, 1))
    couponRate <- if (freqInput == 0) 0 else input$coupon_rate


    spots <- getSplineSpot(maturity, freqInput, spotsKey)

    bondPrice <- priceBond(faceValue, (couponRate / 100), maturity, freqInput, spots)

    bond_ids <- entirePortfolio$bond_id

    bond_id <- ifelse(length(bond_ids) != 0, bond_ids[length(bond_ids)] + 1, 1)

    bondInfoVec <- c(faceValue, as.numeric(couponRate / 100), maturity, freqInput)
    bondInfovec <- as.numeric(bondInfoVec)

    keySensToday <- list(computeKeySens(spots, bondInfoVec) %>%
                        dplyr::rename(!!paste0("Bond ", bond_id, " Delta") := "delta",
                                      !!paste0("Bond ", bond_id, " Gamma") := "gamma"))

    spotsYesterday <- getSplineSpot(maturity, freqInput, spotsKeyYesterday)

    bondPriceYesterday <- priceBond(faceValue, (couponRate / 100), maturity, freqInput, spotsYesterday)

    keySensYesterday <- list(computeKeySens(spotsYesterday, bondInfoVec))

    tempTibble <- tibble(
      "bond_id" = bond_id,
      "face_value" = faceValue,
      "coupon" = couponRate,
      "maturity" = maturity,
      "freq" = frequency,
      "pv" = bondPrice,
      "pv_yest" = bondPriceYesterday,
      "keySens" = keySensToday,
      "keySensYesterday" = keySensYesterday
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
                              "pv" = numeric(),
                              "pv_yest" = numeric(),
                              "keySens" = list(),
                              "keySensYesterday" = list())
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
      dplyr::select(-c(bond_id, keySens, keySensYesterday, pv_yest))

    DT::datatable(
      portfolioTable,
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

  output$portfolio_pie <- renderPlotly({
    req(nrow(portfolio()) > 0)

    portData <- portfolio() %>%
      dplyr::mutate(
        label = paste0("Bond ", bond_id),
        weight = pv / sum(pv)
      )

    plotly::plot_ly(
      portData,
      labels = ~label,
      values = ~weight,
      type   = "pie",
      textinfo = "label+percent"
      ) %>%
      plotly::layout(
        title = "Portfolio Weights by Bond Value",
        showlegend = TRUE
      )
  }) %>%
  bindEvent(input$submit_portfolio)

  output$clear_selected_ui <- renderUI({
    req(input$bond_table_rows_selected > 0)
    actionButton(inputId = "clear_selected_bonds", label = "Clear Selected Bond(s)")
  })

  output$coupon_rate_button <- renderUI({
    req(input$payment_frequency != "Zero-Coupon")

    numericInput(inputId = "coupon_rate", label = "Coupon Rate (%)", min = 0, max = 25, value = 0)
  })

  ## Table of today's exposures

  exposuresToday <- reactive({
    req(nrow(portfolio()) > 0)
    bondTable <- portfolio()

    bondList <- bondTable %>%
      dplyr::select(keySens) %>%
      dplyr::pull(keySens)

    sensDf <- bondList[[1]]

    if (length(bondList) > 1) {
      for (dfIndex in 2:length(bondList)) {

        sensDf <- full_join(sensDf, bondList[[dfIndex]], by = "maturity")
      }
    }

    keyMaturities <- spotsKey$maturity

    sensDf <- sensDf %>%
      mutate(across(everything(), ~replace_na(., 0))) %>%
      dplyr::filter(maturity %in% keyMaturities) %>%
      dplyr::arrange(maturity)

    sensAlone <- sensDf %>%
      dplyr::select(-maturity)

    sensDf <- sensDf %>%
      mutate(Portfolio = pmap(sensAlone, ~ sum(...))) %>%
      dplyr::rename("Time to Maturity" = maturity)

    return(sensDf)

  }) %>%
    bindEvent(input$submit_portfolio)

  output$exposure_table <- render_gt({
    sensitivityTable <- exposuresToday() %>%
      dplyr::select(!!"Time to Maturity", contains("Delta"))

    sensitivityTable %>%
      gt() %>%
      tab_style(
        style = cell_borders(
          sides = "all", color = "black", style = "solid"
        ),
        locations = cells_body()
      ) %>%
      tab_header(
        title = "Bonds + Portfolio Delta Exposures"
      )
    })

  ## PL Attribution vs. Actual

  plAttribCalc <- reactive({
    portTable <- portfolio()

    bondSensYesterday <- portTable %>%
      dplyr::pull(keySensYesterday)

    sensDfYest <- bondSensYesterday[[1]]

    if (length(bondSensYesterday) > 1) {

      for (dfIndex in 2:length(bondSensYesterday)) {

        sensDfYest <- full_join(sensDfYest, bondSensYesterday[[dfIndex]], by = "maturity")
      }

    }

    sensDfYest <- sensDfYest %>%
      mutate(across(everything(), ~replace_na(., 0))) %>%
      dplyr::arrange(maturity)

    sensDfDelta <- sensDfYest %>%
      select(-maturity, -contains("gamma"))

    sensDfGamma <- sensDfYest %>%
      select(-maturity, -contains("delta"))

    sensDfYest <- sensDfYest %>%
      mutate(portfolioDelta = as.numeric(pmap(sensDfDelta, ~ sum(...))),
             portfolioGamma = as.numeric(pmap(sensDfGamma, ~ sum(...)))) %>%
      dplyr::select(maturity, portfolioDelta, portfolioGamma)

    spotSplineToday <- getSplineCurve(spotsKey %>%
                                        dplyr::rename(rate = spotRate))
    spotSplineYesterday <- getSplineCurve(spotsKeyYesterday %>%
                                            dplyr::rename(rate = spotRate))

    predMaturities <- data.frame(
      maturity = sensDfYest$maturity
    )
    spotRatesToday <- spotSplineToday %>%
      stats::predict(
        newdata = predMaturities
      )

    spotRatesYesterday <- spotSplineYesterday %>%
      stats::predict(
        newdata = predMaturities
      )

    spotRatesYest <- tibble(
      maturity = sensDfYest$maturity,
      rate = spotRatesYesterday
    )

    spotRatesTod <- tibble(
      maturity = sensDfYest$maturity,
      rate = spotRatesToday
    )

    calcPnL <- plAttrib(spotRatesYest, spotRatesTod, sensDfYest)

    actualtoday <- portTable %>%
      dplyr::pull(pv) %>%
      sum(.)

    actualYesterday <- portTable %>%
      dplyr::pull(pv_yest) %>%
      sum(.)

    return(calcPnL)
  }) %>%
    bindEvent(input$submit_portfolio)

  output$pnl_by_rate <- render_gt({

    attribPnL <- plAttribCalc()

    attribPnL %>%
      gt()
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
  }) %>%
    bindEvent(input$submit_cor)

  output$loadingsPlot <- renderPlot({
    tidy(pca_estimates, number = 3) %>%
      dplyr::filter(component %in% c("PC1", "PC2", "PC3")) %>%
      dplyr::mutate(terms = as.numeric(terms)) %>%
      ggplot(aes(x = terms, y = value, group = component, col = component)) +
      geom_line() +
      labs(title = "Key Principal Component Loadings",
           x = "Maturity (Years)", y = "Loading") +
      theme_minimal()
  }) %>%
    bindEvent(input$submit_cor)

  output$rolling_vol_plot <- renderPlotly({
    req(input$vol_maturity, input$vol_window)

    plotData <- ir.long %>%
      dplyr::filter(near(maturity, as.numeric(input$vol_maturity), tol = 0.001)) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        date = as.Date(date),
        rolling_vol = slider::slide_dbl(
          ret, sd,
          .before = as.integer(input$vol_window) - 1,
          .complete = TRUE
        ) * sqrt(252) * 10000
      ) %>%
      dplyr::filter(!is.na(rolling_vol))

    req(nrow(plotData) > 0)

    plotly::plot_ly(
      plotData,
      x = ~date, y = ~rolling_vol,
      type = "scatter", mode = "lines",
      line = list(color = "#2c7bb6", width = 1.2)
    ) %>%
      plotly::layout(
        title = paste0("Rolling ", input$vol_window, "-Day Annualised Volatility — ",
                       input$vol_maturity, "Y Treasury"),
        xaxis = list(title = "Date", type = "date"),
        yaxis = list(title = "Annualised Volatility (bps)")
      )
  })%>%
    bindEvent(input$submit_cor)

  output$yield_curve_dynamics <- renderPlotly({
    maturities <- sort(unique(rateData$maturity))
    colors <- colorRampPalette(c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c"))(length(maturities))

    p <- plotly::plot_ly()

    for (i in seq_along(maturities)) {
      mat <- maturities[i]
      matDat <- rateData %>%
        dplyr::filter(maturity == mat) %>%
        dplyr::arrange(date)
      label <- if (mat < 1) paste0(round(mat * 12), "M") else paste0(mat, "Y")
      p <- p %>%
        plotly::add_trace(
          data      = matDat,
          x         = ~date,
          y         = ~rate * 100,
          type      = "scatter",
          mode      = "lines",
          name      = label,
          line      = list(
            color = colors[i],
            width = 1,
            dash  = "dot"
          ),
          hovertemplate = paste0(
            "<b>", label, " Treasury</b><br>",
            "Date: %{x}<br>",
            "Yield: %{y:.2f}%<extra></extra>"
          )
        )
    }

    p %>%
      plotly::layout(
        title = list(
          text = "U.S. Treasury Yield Curve Dynamics",
          font = list(size = 18)
        ),
        xaxis = list(
          title = "Date",
          type  = "date"
        ),
        yaxis = list(
          title = "Yield (%)"
        ),
        legend = list(
          title       = list(text = "Maturity"),
          orientation = "v"
        ),
        hovermode = "x unified"
      )
  })

  output$todays_curve <- renderPlotly({

    todayCurve <- rateData %>%
      dplyr::filter(date == lastDate) %>%
      dplyr::arrange(maturity) %>%
      dplyr::mutate(label = sapply(maturity, function(m) {
        if (m < 1) paste0(round(m * 12), "M") else paste0(m, "Y")
      }))

    plotly::plot_ly(
      todayCurve,
      x = ~label,
      y = ~rate * 100,
      type = "scatter",
      mode = "lines+markers",
      line   = list(color = "#2c7bb7", width = 2),
      marker = list(color = "#2c7bb7", size  = 8)
    ) %>%
      plotly::layout(
        title = paste0("U.S. Treasury Yield Curve — ", format(lastDate, "%B %d, %Y")),
        xaxis = list(title = "Maturity", categoryorder = "array",
                     categoryarray = todayCurve$label),
        yaxis = list(title = "Yield (%)")
      )
  })


}
