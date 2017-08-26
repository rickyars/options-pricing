header <- dashboardHeader(title = "Options Pricing")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Options Chain", tabName = "optionschain", icon = icon("th"))
  )
)

body <- dashboardBody(
  tabItems(
    
    # main dashboard
    tabItem(
      tabName = "dashboard",
      fluidRow(
        column(
          width = 4,
          box(
            title = "Inputs",
            status = "warning",
            solidHeader = TRUE,
            width = NULL,
            textInput("ticker", "Ticker"),
            textInput("strike.price", "Strike Price"),
            dateInput("expiration", "Expiration Date", value = "2018-01-19", format = "yyyy-mm-dd"),
            radioButtons("option.type", "Type", choices = c("Call", "Put"), selected = "Call"),
            numericInput("r", "Annualized rate of interest rate", value = 0.03, step = 0.01),
            numericInput("b", "Annualized cost-of-carry rate", value = 0.03, step = 0.01),
            numericInput("n", "Num. trajectories", value = 100),
            numericInput("days", "Price history (days)", value = 90),
            actionButton("simulate", "Simulate")
          )
        ),
        column(
          width = 8,
          box(
            title = "Random Walk",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            plotlyOutput("plot.bsm.cone")
          ),
          box(
            title = "Implied Probability",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            valueBoxOutput("current"),
            column(
              width = 4,
              textInput("target.price", "Target Price"),
              actionButton("compute", "Compute")
            ),
            valueBoxOutput("implied")
          )
        )
      )
    ),
    
    # options chain
    tabItem(
      tabName = "optionschain",
      dataTableOutput("tbl.options.chain")
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)
