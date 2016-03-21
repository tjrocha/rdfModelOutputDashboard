## app.R ##
rm(list=ls())
library(shiny)
library(shinydashboard)
library(dygraphs)
library(DT)
library(xts)
library(zoo)
library(quantmod)#for testing xts timeseries

############################################################################################
# USER INTERFACE SECTION
############################################################################################
userInterface <- dashboardPage(
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
      c("---", "24-Month Study", "Mid-Term Operations Model (MTOM)", "Colorado River Simulation System (CRSS)")
    ),
    fileInput
    (
      'rdfFileIn', 
      'or choose *.rdf file to upload',
      accept = c('.rdf')
    ),
    selectInput
    (
      "rdfChooser", 
      "Select Output Slot", 
      c(Choose='', state.name),#[JR] this will be dynamically populated based on the model selected"
      selectize = TRUE
    ),
    sidebarMenu
    (
      menuItem("Home", tabName = "home", icon = icon("home")),
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
        tabName = "home",
        fluidRow
        (
        )
      ),
      tabItem
      (
        tabName = "charts",
        h2("Charts and Graphs"),
        "[JR] THIS WILL BE DYNAMICALLY UPDATED WITH CHARTS BASED ON OUTPUT SELECTIONS",
        "MAYBE HAVE SOME ADDITIONAL OPTIONS TO ISOLATE TRACES BY YEAR, TRACE#, SOME THRESHOLD, ETC.",
        br(),br(),
        "Notes: ",
        br(),
        "This page shows some common charts and graphs to view and summarize the selected slot data. ",
        br(),br(),
        #fluidRow
        #(
        #  box #[JR] THIS CONTROL GENERATES AN INPUT FOR THE server SECTION BELOW WHICH IN TURN GENERATES THE PLOT THAT IMMEDIATELY FOLLOWS
        #  (
        #    title="controls", 
        #    sliderInput("slider", "Number of observations:", 1, 100, 50)
        #  )
        #),
        fluidRow #[JR] PLOTS ARE MAPPED TO A PLOT IN THE server SECTION BELOW
        (
          box
          (
            dygraphOutput("plotRdfTS"),
            "Note: Click-and-drag to zoom in. Double-click to undo."
          ), 
          box
          (
            dygraphOutput("plotRdfEnv"),
            "Note: Click-and-drag to zoom in. Double-click to undo.",
            sliderInput
            ("plotEnvelopeRange", label = "Envelope range: ",min = 0, max = 100, value = c(10, 90))
          ) 
        ),
        fluidRow #[JR] PLOTS ARE MAPPED TO A PLOT IN THE server SECTION BELOW
        (
          box
          (
            dygraphOutput("plotRdfPDF"),
            "Note: Click-and-drag to zoom in. Double-click to undo.",
            sliderInput
            ("tsRange", label = "Time series range: ",min = 0, max = 100, value = c(10, 90))
          ), 
          box
          (
            dygraphOutput("plotRdfCDF"),
            "Note: Click-and-drag to zoom in. Double-click to undo.",
            sliderInput
            ("tsRange", label = "Time series range: ",min = 0, max = 100, value = c(10, 90))
          ) 
        )
      ),
      tabItem
      (
        tabName = "data",
        h2("Data Table"),
        "[JR] THIS WILL BE DYNAMICALLY UPDATED WITH DATA GIVEN MODEL AND OUTPUT SELECTIONS",
        br(),br(),
        "Notes:",
        br(),
        "1. Data shown below is based on the selected slot on the sidebar menu. ",
        br(),
        "2. Clicking on the Download Data button will save the entire contents of the table as a ",
        "comma-separated-variables (CSV) file on your local machine. ",
        br(),
        "3. Clicking on the column headers will sort your data in ascending/descending order for that particular column. ",
        br(),
        "4. You may type anything in the search textbox to further filter the results. ",
        br(),  
        "Search Function Examples: Typing '2010' or '2010-01' will filter the data to just ",
        "those for 2010 or January-2010 respectively. ",
        "Typing '1075' will filter data values outside of 1075.00 to 1075.99. ",
        br(),br(),
        downloadButton('downloadData', 'Download Data as a CSV file'),
        br(),br(),
        fluidRow
        (
          div
          (
            style = 'overflow-x: scroll',
            DT::dataTableOutput("tableRdfData") #[JR] tableRdfData IS MAPPED TO A TABLE IN THE server SECTION BELOW
          )
        )
      )
    )
  )
)

############################################################################################
# SERVER SIDE FUNCTIONS, METHODS, AND PROCESSING
############################################################################################
serverProcessing <- function(input, output) 
{
  # DEFINE DYNAMIC VARIABLES HERE
  
  # DEFINE PROCESSING FUNCTIONS AND METHODS HERE
  output$downloadData <- downloadHandler(
    filename = function() 
    {paste('temp',Sys.time(),'.csv', sep='')},
    content = function(filename) 
    {write.csv(data.frame(Date=index(rdfRawData()),coredata(rdfRawData())), filename,row.names = FALSE)}
  )
  
  # POPULATE THE SLOT LIST FROM THE RDF HERE
  output$rdfSlots <- renderPrint(input$rdfChooser)
  
  # GENERATE DATA FROM RDF HERE, THIS IS USED BY ALL THE PROCESSES BELOW
  rdfRawData <- reactive({
    getSymbols(c("GOOG"), from = "2016-01-01")
    GOOG$GOOG.Volume <- NULL
    GOOG$GOOG.Adjusted <- NULL
    GOOG$trace1 <- GOOG$GOOG.Open * rnorm(1,mean=1,sd=0.25)
    GOOG$trace2 <- GOOG$GOOG.Open * rnorm(1,mean=1,sd=0.25)
    GOOG$trace3 <- GOOG$GOOG.Open * rnorm(1,mean=1,sd=0.25)
    GOOG$trace4 <- GOOG$GOOG.Open * rnorm(1,mean=1,sd=0.25)
    GOOG$trace5 <- GOOG$GOOG.Open * rnorm(1,mean=1,sd=0.25)
    GOOG$trace6 <- GOOG$GOOG.Open * rnorm(1,mean=1,sd=0.25)
    GOOG
  })
  
  # GENERATE THE PLOTS HERE
  output$plotRdfTS <- renderDygraph({
    dygraph(rdfRawData(), main = "Time-Series Plot") %>%
    dySeries(attr(rdfRawData,"dimnames")[1]) %>%
    dyLegend(show = "never") %>%
    dyOptions(drawGrid = TRUE)
  })
  
  output$plotRdfEnv <- renderDygraph({
    dygraph(rdfRawData(), main = "Envelope Plot") %>%
      dySeries(attr(rdfRawData,"dimnames")[1]) %>%
      dyLegend(show = "never") %>%
      dyOptions(drawGrid = TRUE)
  })
  
  output$plotRdfPDF <- renderDygraph({
    dygraph(rdfRawData(), main = "Probability Density Plot") %>%
      dySeries(attr(rdfRawData,"dimnames")[1]) %>%
      dyLegend(show = "never") %>%
      dyOptions(drawGrid = TRUE)
  })
  
  output$plotRdfCDF <- renderDygraph({
    dygraph(rdfRawData(), main = "Cumulative Distribution Plot") %>%
      dySeries(attr(rdfRawData,"dimnames")[1]) %>%
      dyLegend(show = "never") %>%
      dyOptions(drawGrid = TRUE)
  })
  
  # GET DATA FROM RDF HERE, see shiny sample #030 for dynamic filtering of the table based on input variables
  output$tableRdfData <- DT::renderDataTable(DT::datatable
  (
    {data.frame(Date=index(rdfRawData()),coredata(rdfRawData()))},
    rownames = FALSE, 
    filter='top', 
    options = list
    (
      pageLength = 10, 
      lengthMenu = c(12, 24, 36, 365)
    )
  ) %>%
  formatStyle('Date',  fontWeight = 'bold')
  )
}

############################################################################################
# GENERATE DASHBOARD INTERFACE
############################################################################################
shinyApp(userInterface, serverProcessing)