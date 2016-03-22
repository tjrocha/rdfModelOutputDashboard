## app.R ##
rm(list=ls())
library(shiny)
library(shinydashboard)
library(dygraphs)
library(DT)
library(xts)
library(zoo)
library(RWDataPlot)

############################################################################################
# USER INTERFACE SECTION
############################################################################################
userInterface <- dashboardPage(
  dashboardHeader
  (
    title="", 
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
      "selectedModel", 
      "1. Select a Model", 
      c("---", "24MS (24-Month Study)", "MTOM (Mid-Term Operations Model)", "CRSS (Colorado River Simulation System)")
    ),
    fileInput
    (
      'rdfFileIn', 
      'or choose *.rdf file to upload',
      accept = c('.rdf')
    ),
    htmlOutput("selectSlotName"),
    sidebarMenu
    (
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Charts", tabName = "charts", icon = icon("area-chart")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Source Code (GitHub Link)", icon = icon("file-code-o"), href = "https://github.com/tjrocha/rdfModelOutputDashboard")
    
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
          h1("RiverWare RDF Model Output Explorer"),
          h2("Instructions"),
          "1. Select a model from the top-most drop down box (ONLY MTOM AND CRSS ARE UPLOADED FOR NOW...).",
          br(),
          "2. Once a model has been selected another drop down box will be populated with the slot names ",
          "present in the selected model RDF file. This may take a few seconds to generate.",
          br(),  
          "3. Once a model and a slot has been selected, you may now view charts and data in their respective ",
          "sections via the sidebar. You may change your selections at any time. ",
          br(),br(),
          h2("Information"),
          "This interface allows users to query, subset, view, and plot RiverWare RDF model outputs. ",
          "This tool is primarily meant to support U.S. Bureau of Reclamation modeling and analysis ",
          "efforts with the 24-Month Study, Mid-Term Operations Model and Colorado River Simulation ",
          "System models. Although the stated purpose is to support USBR, the tool is being developed ",
          "to be as generic as possible so as to enable other users to use is so long as a RiverWare ",
          "*.rdf file is provided.",
          br(),br(),
          "The dashboard uses the following R libraries below and is being developed in RStudio. ",
          br(),
          "shiny <http://shiny.rstudio.com/>",
          br(),
          "shinydashboard <https://rstudio.github.io/shinydashboard/>",
          br(),
          "DT <https://rstudio.github.io/DT/>",
          br(),
          "xts <https://cran.r-project.org/web/packages/xts/index.html>",
          br(),
          "zoo <https://cran.r-project.org/web/packages/zoo/index.html>",
          br(),
          "RWDataPlot <https://github.com/rabutler/RWDataPlot>",
          br(),br(),
          "The source code is also available on GitHub <https://github.com/tjrocha/rdfModelOutputDashboard>"
        )
      ),
      tabItem
      (
        tabName = "charts",
        h2("Charts and Graphs"),
        "Notes:",
        br(),
        "1. Charts shown below are based on the selected model and slot on the sidebar menu. ",
        br(),
        "2. Some plots have interactive elements while some do not. ",
        br(),
        "[JR]: WORK ON THIS SECTION IS CURRENTLY IN PROGRESS",
        br(),br(),
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
        "Notes:",
        br(),
        "1. Data shown below is based on the selected model and slot on the sidebar menu. ",
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
        br(),
        "5. Clicking on the empty boxes below each header shows the range of the data in that particular column ",
        "via a slider bar which you may also use to filter the table rows.",
        br(),br(),
        downloadButton('downloadDataTable', 'Download Data as a CSV file'),
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
  output$downloadDataTable <- downloadHandler(
    filename = function() 
    {paste('temp',Sys.time(),'.csv', sep='')},
    content = function(filename) 
    {write.csv(data.frame(Date=index(rdfRawData()),coredata(rdfRawData())), filename,row.names = FALSE)}
  )
  
  # GET THE SELECTED MODEL FROM THE UI
  selectedModelName <- reactive({
    modelNameString <-input$selectedModel
    modelName <- strsplit(modelNameString," ")[[1]][1]
    modelName
  })
  
  # GENERATE DATA FROM RDF HERE, THIS IS USED BY ALL THE PROCESSES BELOW
  rdfFile <- reactive({
    rdfFileName <- paste(selectedModelName(),".rdf",sep="")#'MTOM.rdf' #'TWS_DNFcurrent.rdf'
    rawRDF <- read.rdf(rdfFileName)
    rawRDF
  })
  
  rdfRawData <- reactive({
    rawRDF <- rdfFile()
    tArray <- rawRDF$runs[[1]]$times
    # OPERATIONS IN ORDER OF EXECUTION
    # 1. rdfSlotToMatrix - read data for 'ithSlotName' string given 'rdf' file
    # 2. cbind - combine datetime and data series arrays
    # 3. data.frame - define R dataframe for conversion to XTS
    # 4. read.zoo - convert dataframe to zoo matrix
    # 5. as.xts - convert zoo matrix to XTS
    # 6. Storage.mode() - convert char values in the XTS matrix to numeric
    rdf <- as.xts(read.zoo(data.frame(cbind(tArray,rdfSlotToMatrix(rawRDF, selectedRDFSlot())))))
    storage.mode(rdf) <- "numeric"
    rdf
  })
  
  # GENERATE THE SLOT SELECTION DROP DOWN LIST
  output$selectSlotName <- renderUI({ 
    selectInput("slotName", "2. Select a slot", c(Choose="",listSlots(rdfFile())))#state.name))#, Selectize = TRUE) 
  })
  
  # GET THE SELECTED SLOT FROM THE UI
  selectedRDFSlot <- reactive({
    slotName <- input$slotName
    slotName
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
  
  # FUNCTION TO PERFORM STATS, AGGREGATION AND ANALYSIS
  output$statsTable <- reactive({
    # DEFINE PERCENTILE VALUES OF INTEREST
    toPctls <- function(rdfRawData) quantile(rdfRawData, c(.10,.25,.50,.75,.90))
    # GET PERCENTILE VALUES OF ENTIRE ARRAY BY EOCY
    eocyPctlXts <- apply.yearly(rdfRawData()[endpoints(rdfRawData(), on="years", k=1)],toPctls)
    # GET PERCENTILE VALUES OF ENTIRE ARRAY BY MONTH
    montPctlXts <- apply.monthly(rdfRawData(),toPctls)
    # GET CY ANNUAL SUMS BY TRACE 
    annlSumsXts <- apply.yearly(rdfRawData(),mean)*12
    # GET PERCENTILE VALUES OF ENTIRE ARRAY BY CY ANNUAL SUMS 
    annlSumsPctlXts <- apply.yearly(annlSumsXts,toPctls)
    # SORT EACH COLUMN DESCENDING FOR MONTHLY VALUE CDF PLOTS
    sortedDataXts <- apply(rdfRawData(),2,sort,decreasing=TRUE)
    # GET PERCENTILE VALUES OF SORTED DATA AT EACH ROW
    sortedDataPctlsXts <- apply(sortedDataXts, 1, toPctls)
    # FLIP ARRAY FOR PLOTTING
    sortedDataPctlsXts <- t(sortedDataPctlsXts[nrow(sortedDataPctlsXts):1,])
    sortedDataPctlsXts <- data.frame(cbind(round((1000*array((1:nrow(sortedDataPctlsXts))/nrow(sortedDataPctlsXts)))+1000,0),sortedDataPctlsXts))
    sortedDataPctlsXts <- as.xts(sortedDataPctlsXts[-1], order.by = as.Date(paste0(sortedDataPctlsXts$V1,"-01-01",format="%Y-01-01")))
    # PACK XTS ARRAYS INTO A LIST AND OUTPUT
    outList <- list(eocyPctlXts,montPctlXts,annlSumsXts,annlSumsPctlXts,sortedDataPctlsXts)
  })
  
}

############################################################################################
# GENERATE DASHBOARD INTERFACE
############################################################################################
shinyApp(userInterface, serverProcessing)