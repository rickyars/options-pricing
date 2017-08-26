function(input, output, session) {
  
  # get quote
  quote <- eventReactive(input$simulate, {
    validate(need(input$ticker != "", "Please input a ticker"))
    getQuote(input$ticker)
  })
  
  # get options chain
  option.chain <- eventReactive(input$simulate, {
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
  
  # get price history
  price.history <- eventReactive(input$simulate, {
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
  
  # show current price
  output$current <- renderValueBox({
    valueBox(
      value = paste0("$", quote()$Last), 
      subtitle = "Current Price", 
      icon = icon("money"),
      color = "light-blue"
    )
  })
  
  # compute implied probability
  implied.probability <- eventReactive(input$compute, {
    df <- trajectories() %>%
      filter(date == input$expiration, bound == "sd.plus")
    sigma <- tail(df$price, 1) - quote()$Last
    p <- pnorm(as.numeric(input$target.price), quote()$Last, sigma)
    round(p, digits = 4) * 100
  })
  
  # show implied probability
  output$implied <- renderValueBox({
    valueBox(
      value = paste0(implied.probability(), "%"), 
      subtitle = "Implied Probability", 
      icon = icon("percent"),
      color = "light-blue"
    )
  })
  
  # print out the options chain (just for funsies)
  output$tbl.options.chain <- renderDataTable(option.chain())
  
}
