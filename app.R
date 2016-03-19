## app.R ##
library(shiny)
library(shinydashboard)
library(dygraphs)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader
  (
    title="Model Output Explorer", 
    dropdownMenu
    (
      type = "notifications",
      notificationItem
      (
        text = "Model disclaimer link",
        icon("users")
      ),
      notificationItem
      (
        text = "Model information link",
        icon("exclamation-triangle")
      )
    )
  ),
  # DASHBOARD SIDEBAR
  dashboardSidebar
  (
    # DEFINE SIDEBAR ITEMS
    selectInput
    (
      "input_type", 
      "Select Model", 
      c("24-Month Study", "Mid-Term Operations Model (MTOM)", "Colorado River Simulation System (CRSS)")
    ),
    selectInput
    (
      "input_type", 
      "Select Output", 
      c("---","[JR] this will be dynamically populated based on the model selected")
    ),
    sidebarMenu
    (
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Charts", tabName = "charts", icon = icon("area-chart")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    )
  ),
  # DASHBOARD BODY
  dashboardBody
  (
    # MAPPED BODY ITEMS TO SIDEBAR
    tabItems
    (
      # BODY PAGE #1
      tabItem
      (
        tabName = "dashboard",
        fluidRow
        (
        )
      ),
      tabItem
      (
        tabName = "charts",
        box
        (
          title="controls", 
          sliderInput("slider", "Number of observations:", 1, 100, 50)
        ),
        box(plotOutput("plot1", height = 250)) #[JR] plot1 IS MAPPED TO A PLOT IN THE server SECTION BELOW
      ),
      tabItem
      (
        tabName = "data",
        "[JR] THIS WILL BE DYNAMICALLY UPDATED WITH DATA GIVEN MODEL AND OUTPUT SELECTIONS",
        fluidRow
        (
          DT::dataTableOutput("tableRdfData") #[JR] tableRdfData IS MAPPED TO A TABLE IN THE server SECTION BELOW
        )
      )
    )
  )
)

server <- function(input, output) 
{
  # DEFINE DYNAMIC VARIABLES HERE
  set.seed(122)
  histdata <- rnorm(500)
  
  # GENERATE THE PLOT HERE
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # GET DATA FROM RDF HERE, see shiny sample #030 for dynamic filtering of the table based on input variables
  output$tableRdfData <- DT::renderDataTable(DT::datatable({
    data <- mpg
    data
  }))
}

shinyApp(ui, server)