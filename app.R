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
          h1("BETA TEST: RiverWare RDF Output Explorer"),
          h2("Instructions"),
          "1. Select a model from the top-most drop down box. Currently in BETA TESTING phase so ",
          "only testing RDF files for MTOM and CRSS have been uploaded to the server. The option ",
          "to upload your own RDF file has not been programmed yet.",
          br(),
          "2. Once a model has been selected, another drop down box will be populated with the slot names ",
          "present in the selected model RDF file. You may click on the drop-down box to specify a slot ",
          "to select or you may type in partial names to filter the available slots in the list. ",
          "The drop-down box may take a few seconds to generate.",
          br(),  
          "3. Once a model and a slot has been selected, you may now view charts and data in their respective ",
          "sections via the sidebar. You may change your selections at any time. ",
          br(),br(),
          h2("Information"),
          "This interface allows users to query, subset, view, and plot RiverWare RDF model outputs. ",
          "This tool is primarily meant to support U.S. Bureau of Reclamation modeling and analysis ",
          "efforts with the 24-Month Study, Mid-Term Operations Model, and Colorado River Simulation ",
          "System models. Although the stated purpose is to support USBR, the tool is being developed ",
          "to be as generic as possible so as to enable other users to use it so long as a RiverWare ",
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
        br(),br(),
        fluidRow #[JR] PLOTS ARE MAPPED TO A PLOT IN THE server SECTION BELOW
        (
            dygraphOutput("plotRdfTS"),
            "Note: Click-and-drag to zoom in. Double-click to undo. You may click on the slider and then use your ",
            "keyboard left and right keys to cycle through each trace.",
            htmlOutput("selectedTrace")
        ),
        br(),br(),
        fluidRow #[JR] PLOTS ARE MAPPED TO A PLOT IN THE server SECTION BELOW
        (
          box
          (
            dygraphOutput("plotRdfEnv"),
            br(),
            "Note: Click-and-drag to zoom in. Double-click to undo. Do not select the 25-50-75 ",
            "percentiles on the slider, these are already shown by the shaded range",
            radioButtons("envChartType", label = "Select aggregation: ", 
                  c("EOCY" = "eocy", "Monthly"="monthly","CY Sum"="cysum"),
                  selected = "monthly",inline = TRUE),
            sliderInput
            ("envChartRange", label = "Envelope range: ",min = 0, max = 100, value = c(10, 90))
          ), 
          box
          (
            dygraphOutput("plotRdfCDF"),
            br(),
            "Note: Click-and-drag to zoom in. Double-click to undo. Do not select the 25-50-75 ",
            "percentiles on the slider, these are already shown by the shaded range",
            sliderInput
            ("excChartRange", label = "Envelope range: ",min = 0, max = 100, value = c(10, 90))
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
  ################################################################################
  # GET THE DATA
  ################################################################################
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
  # GET THE NUMBER OF RUNS
  output$selectedTrace <- renderUI({
    sliderInput("selectedTrace", "Select a trace to highlight: ", min=1, max=as.numeric(rdfFile()$meta$number_of_runs), 
                value=1)
  })
  # GET THE SELECTED TRACE
  sliderTraceSelected <- reactive({
    input$selectedTrace
  })
  
  ################################################################################
  # GENERATE THE CHARTS HERE
  ################################################################################
  # TRACES
  output$plotRdfTS <- renderDygraph({ 
    s1 = paste("V", sliderTraceSelected() + 1, sep="")
    dygraph(rdfRawData(), main = "Raw Time-Series Plot") %>%
    dySeries(attr(rdfRawData,"dimnames")[1]) %>%
    dySeries(s1, label = "Actual", strokeWidth = 3, fillGraph = TRUE) %>%
    dyLegend(show = "never") %>%
    dyOptions(drawGrid = TRUE, colors = "black",strokeWidth = 0.2, strokePattern = "dashed", fillAlpha = .25)
  })
  # ENVELOPE
  output$plotRdfEnv <- renderDygraph({
    #dataEOCYPctls <- envelopeChartData()[[1]]
    #dataMonthlyPctls <- envelopeChartData()[[2]]
    #dataAnnualSumPctl <- envelopeChartData()[[4]]
    s1 = paste(envelopeRangeSelected()[1]*100,"%",sep="")
    s2 = paste(envelopeRangeSelected()[5]*100,"%",sep="")
    data <- envelopeAggSelected()
    dygraph(data, main = "Envelope Plot, Shaded Area is the IQR") %>%
    dySeries(s1, label = "Selected Low Percentile", strokePattern = "dashed", color = "red") %>%
    dySeries(s2, label = "Selected High Percentile", strokePattern = "dashed", color = "blue") %>%
    dySeries(c("25%", "50%", "75%"), label = "Median", strokeWidth = 2, color = "black") %>%
    dyOptions(drawGrid = TRUE)
  })
  # ENVELOPE LOGIC AND OPTIONS
  envelopeAggSelected <- reactive({
    switch(input$envChartType, "eocy" = envelopeChartData()[[1]], "monthly" = envelopeChartData()[[2]], "cysum" = envelopeChartData()[[4]])
  })
  envelopeRangeSelected <- reactive({
    inputRange <- input$envChartRange
    pctlRange <- c(inputRange[1] / 100, 0.25, 0.50, 0.75, inputRange[2] / 100)
  })
  # EXCEEDANCE
  output$plotRdfCDF <- renderDygraph({
    s1 = paste("X",exceedanceRangeSelected()[1]*100,".",sep="")
    s2 = paste("X",exceedanceRangeSelected()[5]*100,".",sep="")
    data <- pctExcChartData()
    dygraph(data, main = "Percent Exceedance Plot, Shaded Area is the IQR", xlab = "Percent Exceedance (%)") %>%
    dySeries(s1, label = "Selected Low Percentile", strokePattern = "dashed", color = "red") %>%
    dySeries(s2, label = "Selected High Percentile", strokePattern = "dashed", color = "blue") %>%
    dySeries(c("X25.", "X50.", "X75."), label = "Median", strokeWidth = 2, color = "black") %>%
    dyOptions(drawGrid = TRUE) %>%
    dyAxis(name="x" , valueFormatter = "function(d){ date = new Date(d); return (date.getFullYear()-1000)/10; }", 
           axisLabelFormatter = "function(d){ return Math.round((d.getFullYear()-1000)/10) }" )
  })
  # EXCEEDANCE LOGIC AND OPTIONS
  exceedanceRangeSelected <- reactive({
    inputRange <- input$excChartRange
    pctlRange <- c(inputRange[1] / 100, 0.25, 0.50, 0.75, inputRange[2] / 100)
  })
  
  ################################################################################
  # GENERATE CHART DATA 
  ################################################################################
  # OLD FUNCTION THAT DID ALL THE STATS, AGGREGATION, AND ANALYSIS
  statsTable <- reactive({
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
  # GENERATE STATS FOR THE ENVELOPE CHART
  envelopeChartData <- reactive({
    # DEFINE PERCENTILE VALUES OF INTEREST
    toPctls <- function(rdfRawData) quantile(rdfRawData, envelopeRangeSelected())
    # GET PERCENTILE VALUES OF ENTIRE ARRAY BY EOCY
    eocyPctlXts <- apply.yearly(rdfRawData()[endpoints(rdfRawData(), on="years", k=1)],toPctls)
    # GET PERCENTILE VALUES OF ENTIRE ARRAY BY MONTH
    montPctlXts <- apply.monthly(rdfRawData(),toPctls)
    # GET CY ANNUAL SUMS BY TRACE 
    annlSumsXts <- apply.yearly(rdfRawData(),mean)*12
    # GET PERCENTILE VALUES OF ENTIRE ARRAY BY CY ANNUAL SUMS 
    annlSumsPctlXts <- apply.yearly(annlSumsXts,toPctls)
    outList <- list(eocyPctlXts,montPctlXts,annlSumsXts,annlSumsPctlXts)
  })
  # GENERATE DATA FOR THE EXCEEDANCE PLOT
  pctExcChartData <- reactive({
    # DEFINE PERCENTILE VALUES OF INTEREST
    toPctls <- function(rdfRawData) quantile(rdfRawData, exceedanceRangeSelected())
    # SORT EACH COLUMN DESCENDING FOR MONTHLY VALUE CDF PLOTS
    sortedDataXts <- apply(rdfRawData(),2,sort,decreasing=TRUE)
    # GET PERCENTILE VALUES OF SORTED DATA AT EACH ROW
    sortedDataPctlsXts <- apply(sortedDataXts, 1, toPctls)
    # FLIP ARRAY FOR PLOTTING
    sortedDataPctlsXts <- t(sortedDataPctlsXts[nrow(sortedDataPctlsXts):1,])
    sortedDataPctlsXts <- data.frame(cbind(round((1000*array((1:nrow(sortedDataPctlsXts))/nrow(sortedDataPctlsXts)))+1000,0),sortedDataPctlsXts))
    sortedDataPctlsXts <- as.xts(sortedDataPctlsXts[-1], order.by = as.Date(paste0(sortedDataPctlsXts$V1,"-01-01",format="%Y-01-01")))
    sortedDataPctlsXts
  })
  
  ################################################################################
  # GENERATE THE DATA TABLE DISPLAY HERE
  ################################################################################
  output$tableRdfData <- DT::renderDataTable(
    DT::datatable
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
      formatStyle
    (
    'Date',  fontWeight = 'bold'
    )
  )
  
  ################################################################################
  # DATA TABLE FUNCTIONS  
  ################################################################################
  # GENERATE DOWNLOAD DATA BUTTON ON THE TABLE TAB
  output$downloadDataTable <- downloadHandler(
    filename = function() 
    {paste('temp',Sys.time(),'.csv', sep='')},
    content = function(filename) 
    {write.csv(data.frame(Date=index(rdfRawData()),coredata(rdfRawData())), filename,row.names = FALSE)}
  )
}

############################################################################################
# GENERATE DASHBOARD INTERFACE
############################################################################################
shinyApp(userInterface, serverProcessing)