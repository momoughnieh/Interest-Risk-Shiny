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

    portDelta <- sensDf %>%
      dplyr::select(maturity, contains("Delta")) %>%
      dplyr::group_by(maturity) %>%
      dplyr::mutate("Portfolio Delta" = sum(across(where(~ is.numeric(.))))) %>%
      dplyr::pull(`Portfolio Delta`)

    sensDf <- sensDf %>%
      mutate("Portfolio Delta" = portDelta) %>%
      dplyr::rename("Time to Maturity" = maturity)

    return(sensDf)

  }) %>%
    bindEvent(input$submit_portfolio)

  output$exposure_table <- render_gt({
    sensitivityTable <- exposuresToday() %>%
      dplyr::select(!!"Time to Maturity", contains("Delta"), !!"Portfolio Delta")

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
    req(nrow(portfolio()) > 0)
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

    return(calcPnL)
  }) %>%
    bindEvent(input$submit_portfolio)

  output$pnl_by_rate <- render_gt({

    attribPnL <- plAttribCalc()

    keyMaturities <- spotsKey$maturity

    attribPnL %>%
      dplyr::filter(maturity %in% keyMaturities) %>%
      dplyr::select(maturity, `Delta PnL`, `Gamma PnL`, `Total PnL`) %>%
      dplyr::rename("Time to Maturity" = maturity) %>%
      gt() %>%
      tab_header(title = "Sensitivites at Select Maturities",
                 subtitle = "** Only select rates displayed. Not a complete picture of PL Attribution **") %>%
      tab_style(
        style = cell_borders(sides = "all", color = "black"),
        location = cells_body()
      )
  })

  pnlInstrument <- reactive({

    req(nrow(portfolio()) > 0)

    portTable <- portfolio()
    fullSens <- plAttribCalc()

    actualtoday <- portTable %>%
      dplyr::pull(pv) %>%
      sum(.)

    actualYesterday <- portTable %>%
      dplyr::pull(pv_yest) %>%
      sum(.)

    actualPnL <- round(actualtoday - actualYesterday, 2)

    attribPnL <- fullSens %>%
      dplyr::pull(`Total PnL`) %>%
      sum(.)
    attribPnL <- round(attribPnL, 2)

    unexplainedPnL <- round(actualPnL - attribPnL, 2)

    pnlTable <- tibble(
      "PnL Type" = c("Actual PnL", "Attributed PnL", "Unexplained PnL"),
      "PnL ($)" = c(actualPnL, attribPnL, unexplainedPnL)
    )


  }) %>%
    bindEvent(input$submit_portfolio)

  output$pnl_by_instrument <- render_gt({

    pnlInstrumentTable <- pnlInstrument()

    pnlInstrumentTable %>%
      gt() %>%
      tab_header(title = "Actual vs. Attributed PnL", subtitle = "For the total portfolio") %>%
      tab_style(
        style = cell_borders(sides = "all", color = "black"),
        locations = cells_body()
      )

  })

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
      line = list(color = "blue", width = 1.2)
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
    colors <- colorRampPalette(c("blue", "lightblue", "orange", "red"))(length(maturities))

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
      line   = list(color = "blue", width = 2),
      marker = list(color = "blue", size  = 8)
    ) %>%
      plotly::layout(
        title = paste0("U.S. Treasury Yield Curve — ", format(lastDate, "%B %d, %Y")),
        xaxis = list(title = "Maturity", categoryorder = "array",
                     categoryarray = todayCurve$label),
        yaxis = list(title = "Yield (%)")
      )
  })

  corr_data <- reactive({
    req(input$corr_selection, input$corr_date_selection)
    selected_maturities <- as.numeric(input$corr_selection)
    ir.wide %>%
      dplyr::filter(
        date >= input$corr_date_selection[1],
        date <= input$corr_date_selection[2]
      ) %>%
      na.omit() %>%
      dplyr::select(any_of(as.character(selected_maturities)))
  }) %>%
    bindEvent(input$submit_corr)

  corrMatrix <- reactive({
    req(corr_data())
    matrix <- corr_data() %>%
      stats::cor(method = "kendall")
    labels <- sapply(as.numeric(colnames(matrix)), function(m) {
      if (m < 1) paste0(round(m * 12), "M") else paste0(m, "Y")
    })
    colnames(matrix) <- labels
    rownames(matrix) <- labels
    return(matrix)
  }) %>%
    bindEvent(input$submit_corr)

  output$corr_table <- render_gt({
    req(corrMatrix())

    matrix1 <- corrMatrix()

    as.data.frame(matrix1) %>%
      tibble::rownames_to_column("Maturity") %>%
      gt() %>%
      tab_header(title = paste0("Correlation Matrix - ", input$corr_date_selection[1], " to ", input$corr_date_selection[2])) %>%
      fmt_number(
        columns = -Maturity,
        decimals = 2
      ) %>%
      data_color(
        columns  = -Maturity,
        palette  = c("green", "yellow", "blue"),
        domain   = c(-1, 1),
        na_color = "white"
      ) %>%
      tab_style(
        style     = cell_borders(sides = "all", color = "black", style = "solid"),
        locations = cells_body()
      ) %>%
      cols_align(align = "center", columns = -Maturity)
  }) %>%
    bindEvent(input$submit_corr)

}
