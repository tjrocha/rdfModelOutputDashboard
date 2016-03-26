############################################################################################
# This application was built in RStudio by Jon Rocha at jrocha@usbr.gov
#
# The application allows users to query, subset, view, and plot RiverWare RDF model outputs. 
# This is primarily meant to support U.S. Bureau of Reclamation (USBR) modeling and analysis 
# efforts with the 24-Month Study, Mid-Term Operations Model, and Colorado River Simulation 
# System models. Although the stated purpose is to support USBR, the tool is being developed 
# to be as generic as possible so as to enable other users to use it so long as a RiverWare 
# *.rdf file is provided. 
#
# Application is distributed with an MIT license, March 2016
############################################################################################

############################################################################################
# LOAD REQUIRED PACKAGES
############################################################################################
rm(list=ls())
library(shiny)
library(shinydashboard)
library(dygraphs)
library(DT)
library(xts)
library(zoo)
library(RWDataPlot)
############################################################################################
# SERVER SIDE FUNCTIONS, METHODS, AND PROCESSING
############################################################################################
serverProcessing <- function(input, output, clientData, session)
{
  # INCREASE SHINY UPLOAD SIZE TO 30MB
  options(shiny.maxRequestSize=30*1024^2) 
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
    if (!is.null(input$rdfFileIn) && selectedModelName() == input$rdfFileIn$name){
      rdfFileName <- input$rdfFileIn$datapath
    }
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
    rdf <- as.xts(read.zoo(data.frame(cbind(tArray,rdfSlotToMatrix(rawRDF, selectedRDFSlot())))),na.rm=TRUE)
    storage.mode(rdf) <- "numeric"
    rdf
  })
  # GENERATE THE MODEL SELECTION DROP DOWN LIST
  output$selectModelName <- renderUI({
    input$rdfFileIn
    if (is.null(input$rdfFileIn)){
      selectInput(
        "selectedModel", "1. Select a Model", 
        c("---", "24MS (24-Month Study)", 
          "MTOM (Mid-Term Operations Model)", 
          "CRSS (Colorado River Simulation System)")
      )
    }
    else{
      selectInput(
        "selectedModel", "1. Select a Model", 
        c(input$rdfFileIn$name, "24MS (24-Month Study)", 
          "MTOM (Mid-Term Operations Model)", 
          "CRSS (Colorado River Simulation System)")
      )
    }
  })
  # GENERATE THE SLOT SELECTION DROP DOWN LIST
  output$selectSlotName <- renderUI({ 
    selectInput(
      "slotName", "2. Select a slot", c(Choose="",listSlots(rdfFile())))
  })
  # GENERATE THE FILE SELECTION BUTTON
  output$resettableFileInput <- renderUI({
    input$selectedModel
    fileInput('rdfFileIn', 'or upload an *.rdf file', accept = c('.rdf'))
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
  # GET RDF AND SLOT INFO
  ################################################################################
  # GET SELECTED RDF INFORMATION
  getRdfInfo <- reactive({
    rdfInfo <- data.frame(matrix(NA, nrow = 8, ncol = 2))
    labels <- c(
      "Start", "End", "Interval", "#Runs", "Run Name", "Run By","Run Date","Run Rules"
    )
    info <- c(
      rdfFile()$runs[[1]]$start, # RW START DATE
      rdfFile()$runs[[1]]$end, # RW END DATE
      rdfFile()$runs[[1]]$time_step_unit, # MODEL TIME STEP
      rdfFile()$meta$number_of_runs, # N-RUNS
      rdfFile()$meta$name, # MRM RUN NAME
      rdfFile()$meta$owner, # USER NAME
      rdfFile()$meta$create_date, # MODEL RUN DATE
      rdfFile()$runs[[1]]$rule_set # SELECTED RULES
    )
    rdfInfo[,1] <- labels
    rdfInfo[,2] <- info
    rdfInfo <- data.frame(rdfInfo)
    names(rdfInfo) <- c("from","message")
    rdfInfo
  })
  # GET SELECTED SLOT INFORMATION
  getSlotInfo <- reactive({
    slotInfo <- data.frame(matrix(NA, nrow = 5, ncol = 2))
    labels <- c(
      "Units", "Scale", "Object", "Object Name", "Object Slot"
    )
    info <- c(
      eval(parse(text=paste("rawRDF$runs[[1]]$objects$'",selectedRDFSlot(),"'$units",sep=""))),
      eval(parse(text=paste("rawRDF$runs[[1]]$objects$'",selectedRDFSlot(),"'$scale",sep=""))),
      eval(parse(text=paste("rawRDF$runs[[1]]$objects$'",selectedRDFSlot(),"'$object_type",sep=""))),
      eval(parse(text=paste("rawRDF$runs[[1]]$objects$'",selectedRDFSlot(),"'$object_name",sep=""))),
      eval(parse(text=paste("rawRDF$runs[[1]]$objects$'",selectedRDFSlot(),"'$slot_name",sep="")))
    )
    slotInfo[,1] <- labels
    slotInfo[,2] <- info
    slotInfo <- data.frame(slotInfo)
    names(slotInfo) <- c("from","message")
    slotInfo
  })
  output$rdfInfoMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumesslotInfoMenu
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(getRdfInfo(), 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
  output$slotInfoMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(getSlotInfo(), 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
  ################################################################################
  # GENERATE THE CHARTS HERE
  ################################################################################
  # TRACES
  output$plotRdfTS <- renderDygraph({ 
    if (rdfFile()$meta$number_of_runs == 1){
      s1 = "V1"
    }
    else {
      s1 = paste("V", sliderTraceSelected() + 1, sep="")
    }
    dygraph(rdfRawData(), main = "Raw Time-Series Plot") %>%
      dySeries(attr(rdfRawData,"dimnames")[1]) %>%
      dySeries(s1, label = "Actual", strokeWidth = 3, fillGraph = TRUE) %>%
      dyLegend(show = "never") %>%
      dyOptions(drawGrid = TRUE, colors = "black",strokeWidth = 0.2, strokePattern = "dashed", fillAlpha = .25)
  })
  # ENVELOPE
  output$plotRdfEnv <- renderDygraph({
    s1 = paste(envelopeRangeSelected()[1]*100,"%",sep="")
    s2 = paste(envelopeRangeSelected()[5]*100,"%",sep="")
    data <- envelopeAggSelected()
    dygraph(data, main = "Envelope Plot, Shaded Area is the IQR") %>%
      dySeries(s1, label = "Selected Low Percentile", strokePattern = "dashed", color = "red") %>%
      dySeries(s2, label = "Selected High Percentile", strokePattern = "dashed", color = "blue") %>%
      dySeries(c("25%", "50%", "75%"), label = "Median", strokeWidth = 2, color = "black") %>%
      dyOptions(drawGrid = TRUE) %>%
      dyLegend(show = "auto", width = 300)
  })
  # ENVELOPE LOGIC AND OPTIONS
  envelopeAggSelected <- reactive({
    switch(input$envChartType, "eocy" = envelopeChartData()[[1]], "monthly" = envelopeChartData()[[2]], "cysum" = envelopeChartData()[[4]])
  })
  envelopeRangeSelected <- reactive({
    inputRange <- input$envChartRange
    if (inputRange[1] == 25||inputRange[1] == 50 ||inputRange[1] == 75 || inputRange[1] == inputRange[2])
      inputRange[1] = inputRange[1] - 1
    if (inputRange[2] == 25||inputRange[2] == 50 ||inputRange[2] == 75)
      inputRange[2] = inputRange[2] + 1
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
      dyOptions(drawGrid = TRUE)  %>%
      dyLegend(show = "auto", width = 300) %>%
      dyAxis(name="x" , valueFormatter = "function(d){ date = new Date(d); return (date.getFullYear()-1000)/10; }", 
             axisLabelFormatter = "function(d){ return Math.round((d.getFullYear()-1000)/10) }" )
  })
  # EXCEEDANCE LOGIC AND OPTIONS
  exceedanceRangeSelected <- reactive({
    inputRange <- input$excChartRange
    if (inputRange[1] == 25||inputRange[1] == 50 ||inputRange[1] == 75 || inputRange[1] == inputRange[2])
      inputRange[1] = inputRange[1] - 1
    if (inputRange[2] == 25||inputRange[2] == 50 ||inputRange[2] == 75)
      inputRange[2] = inputRange[2] + 1
    pctlRange <- c(inputRange[1] / 100, 0.25, 0.50, 0.75, inputRange[2] / 100)
  })
  # THRESHOLD
  output$plotRdfThreshCheck <- renderDygraph({
    data <- thresoldChartData()
    dygraph(data, main = "Percent Of Traces That Meet A Threshold") %>%
      dyOptions(drawGrid = TRUE)  %>%
      dyLegend(show = "auto", width = 300) 
  })
  ################################################################################
  # GENERATE CHART DATA 
  ################################################################################
  # GENERATE STATS FOR THE ENVELOPE CHART
  envelopeChartData <- reactive({
    # DEFINE PERCENTILE VALUES OF INTEREST
    toPctls <- function(rdfRawData) quantile(rdfRawData, envelopeRangeSelected(), na.rm=TRUE)
    # GET PERCENTILE VALUES OF ENTIRE ARRAY BY EOCY
    eocyPctlXts <- apply.yearly(rdfRawData()[endpoints(rdfRawData(), on="years", k=1)],toPctls)
    # GET PERCENTILE VALUES OF ENTIRE ARRAY BY MONTH
    montPctlXts <- apply.monthly(rdfRawData(),toPctls)
    # GET CY ANNUAL SUMS BY TRACE 
    annlSumsXts <- apply.yearly(rdfRawData(),colSums)
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
  # GENERATE DATA FOR THE THRESHOLD CHECK
  thresoldChartData <- reactive({
    # GENERATE DATA BASED ON THRESHOLD TYPE
    threshData <- input$threshDataType
    if (threshData == "eocy")
      rdfXTS <- rdfRawData()[.indexmon(rdfRawData()) == 11]
    else if (threshData == "cysum")
      rdfXTS <- apply.yearly(rdfRawData(),colSums)
    else if (threshData == "none")
      rdfXTS <- rdfRawData()
    else
      stop("Not a valid threshold comparison option")
    # GET THRESHOLD VALUES
    valuesList <- as.numeric(unlist(strsplit(input$threshValue,",")))
    for (ithVal in 1:length(valuesList)){
      valueIn <- valuesList[ithVal]
      # DETERMINE COMPARISON TYPE AND GET A BOOLEAN ARRAY OF VALUES THAT MEET THE THRESHOLD
      comparison <- input$threshCompType
      if (comparison == "GT"){
        boolArray <- rdfXTS > valueIn  
        ithName <- "GreaterThan"
      }
      else if (comparison == "LT"){
        boolArray <- rdfXTS < valueIn 
        ithName <- "LessThan"
      }
      else
        stop(paste(comparison, " is not a valid input. Use GT for greater than or LT for less than", sep=""))
      ithName <- paste(ithName,valueIn,sep="")
      # GET A COUNT OF TRUE VALUES AT EACH COLUMN FOR EACH ROW
      if (ithVal == 1){
        trueCount <- xts(rowSums(boolArray),index(boolArray))
      }
      else{
        trueCountTemp <- xts(rowSums(boolArray),index(boolArray))
        trueCount <- merge(trueCount,trueCountTemp)
      }
      colnames(trueCount)[ithVal] <- ithName
    }
    # GET THE TOTAL COUNT OF COLUMNS
    totalCount <- length(dimnames(boolArray)[[2]])
    # RETURN PERCENTAGE OF VALUES THAT MEET THE COMPARISON TYPE
    thresholdPctgs <- trueCount/totalCount * 100
    thresholdPctgs
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
      options = list(pageLength = 10, lengthMenu = c(12, 24, 36, 365))
    ) %>%
    formatStyle
    ('Date', fontWeight = 'bold')
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