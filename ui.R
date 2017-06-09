pageWithSidebar(
  headerPanel('BSM Cones'),
  sidebarPanel(
    textInput("ticker", "Ticker"),
    textInput("strike.price", "Strike Price"),
    dateInput("expiration", "Expiration Date", value = "2018-01-19", format = "yyyy-mm-dd"),
    radioButtons("option.type", "Type", choices = c("Call", "Put"), selected = "Call"),
    numericInput("r", "Annualized rate of interest rate", value = 0.03, step = 0.01),
    numericInput("b", "Annualized cost-of-carry rate", value = 0.03, step = 0.01),
    numericInput("n", "Num. trajectories", value = 100),
    numericInput("days", "Price history (days)", value = 90),
    actionButton("run", "Run")
  ),
  mainPanel(
    plotlyOutput("plot.bsm.cone"),
    dataTableOutput("tbl.options.chain")
  )
)
