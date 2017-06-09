function(input, output, session) {
  
  # get quote
  quote <- eventReactive(input$run, {
    validate(need(input$ticker != "", "Please input a ticker"))
    getQuote(input$ticker)
  })
  
  # get options chain
  option.chain <- eventReactive(input$run, {
    validate(need(input$ticker != "", "Please input a ticker"))
    option.chain <- getOptionChain(input$ticker, Exp = input$expiration)
    
    if (input$option.type == "Call") 
    {
      return(option.chain$calls)
    }
    
    if (input$option.type == "Put") 
    {
      return(option.chain$puts)
    }
  })
  
  price.history <- eventReactive(input$run, {
    GetPriceHistory(input$ticker, input$days)
  })
  
  # compute volatility 
  volatility <- reactive({
    validate(
      need(!is.na(as.numeric(input$strike.price)), "Strike price must be a number")
    )
    GetVolatility(quote(), as.numeric(input$strike.price), option.chain(), input$expiration, input$option.type, input$r, input$b)
  })
  
  # simulate trajectories
  trajectories <- reactive({
    SimulateTrajectories(quote(), input$r, volatility(), input$expiration, input$n)
  })
  
  # generate BSM cone plot
  output$plot.bsm.cone <- renderPlotly({
    # need to add today to the historical
    history <- price.history() %>%
      bind_rows(data_frame(date = Sys.Date(), price = quote()$Last))
    
    plot_ly(data = history, x = ~date, y = ~price, name = "past", type = 'scatter', mode = 'lines') %>%
      add_trace(data = trajectories(), x = ~date, y = ~price, color = ~bound, name = "projection")
  })
  
  # print out the options chain (just for funsies)
  output$tbl.options.chain <- renderDataTable(option.chain())
  
}
