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
source('global.R')
############################################################################################
# SERVER SIDE FUNCTIONS, METHODS, AND PROCESSING
############################################################################################
serverProcessing <- function(input, output, clientData, session){
  # INCREASE SHINY UPLOAD SIZE TO 100MB
  options(shiny.maxRequestSize=100*1024^2) 
################################################################################
# GET THE DATA
################################################################################
  # GET THE SELECTED MODEL FROM THE UI
  selectedModelName <- reactive({
    modelNameString <-input$selectedModel
    modelName <- strsplit(modelNameString," ")[[1]][1]
    modelName
  })
  # READ THE RDF FILE
  rdfFile <- reactive({
#     rdfFileName <- paste(selectedModelName(),".rdf",sep="")#'MTOM.rdf' #'TWS_DNFcurrent.rdf'
#     if (!is.null(input$rdfFileIn) && selectedModelName() == input$rdfFileIn$name){
#       rdfFileName <- input$rdfFileIn$datapath
#     }
#     rawRDF <- read.rdf(rdfFileName)
    rdfInt <- as.numeric(strsplit(selectedModelName(),split="\\.")[[1]][1])
    if (rdfInt == 1)
      rawRDF <- newMppeData
    else 
      rawRDF <- oldMppeData
    rawRDF
  })
  # READ THE SLOT DATA FROM RDF
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
    newRunNames <- c()
    for (ithRun in c(1:as.numeric(rawRDF$meta$number_of_runs))){
      newRunNames <- c(newRunNames, paste('Trace',ithRun,sep=""))
    }
    names(rdf) <- newRunNames
    rdf
  })
  # GENERATE THE MODEL SELECTION DROP DOWN LIST
  output$selectModelName <- renderUI({
    input$rdfFileIn
    if (is.null(input$rdfFileIn)){
      selectInput(
        "selectedModel", "1. Select a Run", 
        c(Choose="", paste("1.",newRunName(), "CRSS Run",sep=" "), paste("2.",oldRunName(), "CRSS Run",sep=" "))
      )
    }
    else{
      selectInput(
        "selectedModel", "1. Select an RDF", 
        c(input$rdfFileIn$name, "newMPPE")
      )
    }
  })
  # GENERATE THE SLOT SELECTION DROP DOWN LIST
  output$selectSlotName <- renderUI({ 
    validate(need(selectedModelName() != "", ''))
    selectInput(
      "slotName", "2. Select a slot", c(Choose="",sort(listSlots(rdfFile()))))
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
    validateSelectedModel()
    sliderInput("selectedTrace", "Select a trace to highlight: ", min=1, max=as.numeric(rdfFile()$meta$number_of_runs), 
                value=1)
  })
  # GET THE SELECTED TRACE
  sliderTraceSelected <- reactive({
    input$selectedTrace - 1
  })
  # VALIDATE SELECTED MODEL
  validateSelectedModel <- reactive({
    validate(need(selectedModelName() != "", 'Select a Run...'))
  })
  # VALIDATE SELECTED SLOT
  validateSelectedSlot <- reactive({
    validate(need(selectedRDFSlot() != "", 'Select a Slot from the drop down list...'))
  })
################################################################################
# GET RDF AND SLOT INFO
################################################################################
  # GET SELECTED RDF INFORMATION
  getRdfInfo <- reactive({
    rdfInfo <- data.frame(matrix(NA, nrow = 7, ncol = 2))
    labels <- c(
      "Start", "End", "Interval", "#Runs", "Run Name", "Run By","Run Date"
    )
    info <- c(
      rdfFile()$runs[[1]]$start, # RW START DATE
      rdfFile()$runs[[1]]$end, # RW END DATE
      rdfFile()$runs[[1]]$time_step_unit, # MODEL TIME STEP
      rdfFile()$meta$number_of_runs, # N-RUNS
      rdfFile()$meta$name, # MRM RUN NAME
      rdfFile()$meta$owner, # USER NAME
      rdfFile()$meta$create_date # MODEL RUN DATE
    )
    rdfInfo[,1] <- labels
    rdfInfo[,2] <- info
    rdfInfo <- data.frame(rdfInfo)
    names(rdfInfo) <- c("item","message")
    rdfInfo
  })
  # GET SELECTED SLOT INFORMATION
  getSlotInfo <- reactive({
    rdfFile <- rdfFile()
    slotInfo <- data.frame(matrix(NA, nrow = 5, ncol = 2))
    labels <- c(
      "Units", "Scale", "Object", "Object Name", "Object Slot"
    )
    info <- c(
      eval(parse(text=paste("rdfFile$runs[[1]]$objects$'",selectedRDFSlot(),"'$units",sep=""))),
      eval(parse(text=paste("rdfFile$runs[[1]]$objects$'",selectedRDFSlot(),"'$scale",sep=""))),
      eval(parse(text=paste("rdfFile$runs[[1]]$objects$'",selectedRDFSlot(),"'$object_type",sep=""))),
      eval(parse(text=paste("rdfFile$runs[[1]]$objects$'",selectedRDFSlot(),"'$object_name",sep=""))),
      eval(parse(text=paste("rdfFile$runs[[1]]$objects$'",selectedRDFSlot(),"'$slot_name",sep="")))
    )
    slotInfo[,1] <- labels
    slotInfo[,2] <- info
    slotInfo <- data.frame(slotInfo)
    names(slotInfo) <- c("item","message")
    slotInfo
  })
  getChartLabel <- reactive({
    rdfFile <- rdfFile()
    label <- paste(
      eval(parse(text=paste("rdfFile$runs[[1]]$objects$'",selectedRDFSlot(),"'$object_name",sep=""))),
      eval(parse(text=paste("rdfFile$runs[[1]]$objects$'",selectedRDFSlot(),"'$slot_name",sep=""))),
      sep = " "
    )
    label <- paste(label, " (",
      if (eval(parse(text=paste("rdfFile$runs[[1]]$objects$'",selectedRDFSlot(),"'$scale",sep=""))) != 1)
        {eval(parse(text=paste("rdfFile$runs[[1]]$objects$'",selectedRDFSlot(),"'$scale",sep="")))},
      eval(parse(text=paste("rdfFile$runs[[1]]$objects$'",selectedRDFSlot(),"'$units",sep=""))),
      ")", sep=""
    )
    label
  })
################################################################################
# GENERATE THE GRAPHS HERE
################################################################################
  # TRACES
  output$plotRdfTS <- renderDygraph({ 
    validateSelectedModel()
    validateSelectedSlot()
    if (rdfFile()$meta$number_of_runs == 1){
      s1 = "Trace1"
    }
    else {
      s1 = paste("Trace", sliderTraceSelected() + 1, sep="")
    }
    tsDygraph() %>%
      dySeries(s1, label = "Actual", strokeWidth = 3, fillGraph = TRUE)
  })
  tsDygraph <- reactive({
    dygraph(rdfRawData(), main = "Raw Time-Series Plot") %>%
      dySeries(attr(rdfRawData,"dimnames")[1]) %>%
      dyLegend(show = "never") %>%
      dyAxis(name="y", label=getChartLabel()) %>%
      dyOptions(drawGrid = TRUE, colors = "black",strokeWidth = 0.2, strokePattern = "dashed", fillAlpha = .25)
  })
  # ENVELOPE
  output$plotRdfEnv <- renderDygraph({
    validateSelectedModel()
    validateSelectedSlot()
    envDygraph()
  })
  envDygraph <- reactive({
    s1 = paste(envelopeRangeSelected()[1]*100,"%",sep="")
    s2 = paste(envelopeRangeSelected()[5]*100,"%",sep="")
    data <- envelopeAggSelected()
    dygraph(data, main = "Envelope Plot") %>%
      dySeries(s1, label = "Selected Low Percentile", strokePattern = "dashed", color = "red") %>%
      dySeries(s2, label = "Selected High Percentile", strokePattern = "dashed", color = "blue") %>%
      dySeries(c("25%", "50%", "75%"), label = "Median", strokeWidth = 2, color = "black") %>%
      dyOptions(drawGrid = TRUE) %>%
      dyAxis(name="y", label=getChartLabel()) %>%
      dyLegend(labelsDiv = "plotRdfEnvLegend")
  })
  # ENVELOPE LOGIC AND OPTIONS
  envelopeAggSelected <- reactive({
    switch(input$envChartType, "eocy" = envelopeChartData()[[1]], "monthly" = envelopeChartData()[[2]], "cysum" = envelopeChartData()[[4]])
  })
  envelopeRangeSelected <- reactive({
    inputRange <- input$envChartRange
    if (inputRange[1] == 25||inputRange[1] == 50 ||inputRange[1] == 75 || inputRange[1] == inputRange[2])
      inputRange[1] = inputRange[1] - 0.01
    if (inputRange[2] == 25||inputRange[2] == 50 ||inputRange[2] == 75)
      inputRange[2] = inputRange[2] + 0.01
    pctlRange <- c(inputRange[1] / 100, 0.25, 0.50, 0.75, inputRange[2] / 100)
  })
  # THRESHOLD
  output$plotRdfThreshCheck <- renderDygraph({
    validateSelectedModel()
    validateSelectedSlot()
    data <- thresoldChartData()
    dygraph(data, main = "Percent Of Traces That Meet A Threshold") %>%
      dyOptions(drawGrid = TRUE)  %>%
      dyLegend(labelsDiv = "plotRdfThreshCheckLegend")
  })
################################################################################
# GENERATE CHART DATA 
################################################################################
  # GENERATE STATS FOR THE ENVELOPE CHART
  output$downloadEnvelopeAggSelectedData <- downloadHandler(
    filename = function() 
    {paste('temp',Sys.time(),'.csv', sep='')},
    content = function(filename) 
    {write.csv(data.frame(Date=index(envelopeAggSelected()),coredata(envelopeAggSelected())), filename,row.names = FALSE)}
  )
  output$downloadThresoldChartDataData <- downloadHandler(
    filename = function() 
    {paste('temp',Sys.time(),'.csv', sep='')},
    content = function(filename) 
    {write.csv(data.frame(Date=index(thresoldChartData()),coredata(thresoldChartData())), filename,row.names = FALSE)}
  )
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
  output$tableRdfData <- DT::renderDataTable({
    validateSelectedModel()
    validateSelectedSlot()
    data <- data.frame(Date=index(rdfRawData()),coredata(rdfRawData()))
    DT::datatable(data, 
                  options = list(pageLength = 10, lengthMenu = c(12, 24, 36, 365)),
                  filter = 'top',
                  rownames = FALSE) 
  })
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
################################################################################
# GENERATE CRSS STANDARD CHARTS 
################################################################################
  newRunName <- reactive({
    format(as.Date(newMppeData$meta$create_date),format="%b%Y")
  })
  oldRunName <- reactive({
    format(as.Date(oldMppeData$meta$create_date),format="%b%Y")
  })
  output$meadStandardGraphXts <- renderDygraph({
    if(input$meadStandardCompare){
      data <- getResXtsData('mead','combined')
      graph <- dygraph(data, main = "Lake Mead EOCY Elevation Percentiles") %>%
        dySeries("X10.", label = paste(newRunName(), " 10th", sep=""), strokeWidth = 3, strokePattern = "dotted", color = "#244A9F") %>%
        dySeries("X50.", label = paste(newRunName(), " 50th", sep=""), strokeWidth = 3, color = "#244A9F") %>%
        dySeries("X90.", label = paste(newRunName(), " 90th", sep=""), strokeWidth = 3, strokePattern = "dashed", color = "#244A9F") %>%
        dySeries("X10..1", label = paste(oldRunName(), " 10th", sep=""), strokeWidth = 3, strokePattern = "dotted", color = "#CB9F5B") %>%
        dySeries("X50..1", label = paste(oldRunName(), " 50th", sep=""), strokeWidth = 3, color = "#CB9F5B") %>%
        dySeries("X90..1", label = paste(oldRunName(), " 90th", sep=""), strokeWidth = 3, strokePattern = "dashed", color = "#CB9F5B") %>%
        dyOptions(drawGrid = TRUE) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.25) %>%
        dyAxis(name="y", label="Lake Elevation (feet above MSL)") %>%
        dyAxis(name="x", label="Year", 
               valueFormatter = 'function(d){date = new Date(d); return date.getFullYear();}', 
               axisLabelFormatter = 'function(d){date = new Date(d); return (date.getFullYear()-1);}') %>%
        dyLegend(labelsDiv = "meadStandardGraphXtsLegend")
    }
    else {
      data <- getResXtsData('mead','new')
      graph <- dygraph(data, main = "Lake Mead EOCY Elevation Percentiles") %>%
        dySeries("10%", label = "10th", strokeWidth = 3, strokePattern = "dotted", color = "#244A9F") %>%
        dySeries("50%", label = "50th", strokeWidth = 3, color = "#244A9F") %>%
        dySeries("90%", label = "90th", strokeWidth = 3, strokePattern = "dashed", color = "#244A9F") %>%
        dyOptions(drawGrid = TRUE) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1.0) %>%
        dyAxis(name="y", label="Lake Elevation (feet above MSL)") %>%
        dyAxis(name="x", label="Year", 
               valueFormatter = 'function(d){date = new Date(d); return date.getFullYear();}', 
               axisLabelFormatter = 'function(d){date = new Date(d); return (date.getFullYear()-1);}') %>%
        dyLegend(labelsDiv = "meadStandardGraphXtsLegend")
    }
  })
  output$powellStandardGraphXts <- renderDygraph({
    if(input$powellStandardCompare){
      data <- getResXtsData('powell','combined')
      graph <- dygraph(data, main = "Lake Powell EOCY Elevation Percentiles") %>%
        dySeries("X10.", label = paste(newRunName(), " 10th", sep=""), strokeWidth = 3, strokePattern = "dotted", color = "#244A9F") %>%
        dySeries("X50.", label = paste(newRunName(), " 50th", sep=""), strokeWidth = 3, color = "#244A9F") %>%
        dySeries("X90.", label = paste(newRunName(), " 90th", sep=""), strokeWidth = 3, strokePattern = "dashed", color = "#244A9F") %>%
        dySeries("X10..1", label = paste(oldRunName(), " 10th", sep=""), strokeWidth = 3, strokePattern = "dotted", color = "#CB9F5B") %>%
        dySeries("X50..1", label = paste(oldRunName(), " 50th", sep=""), strokeWidth = 3, color = "#CB9F5B") %>%
        dySeries("X90..1", label = paste(oldRunName(), " 90th", sep=""), strokeWidth = 3, strokePattern = "dashed", color = "#CB9F5B") %>%
        dyOptions(drawGrid = TRUE) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.25) %>%
        dyAxis(name="y", label="Lake Elevation (feet above MSL)") %>%
        dyAxis(name="x", label="Year", 
               valueFormatter = 'function(d){date = new Date(d); return date.getFullYear();}', 
               axisLabelFormatter = 'function(d){date = new Date(d); return (date.getFullYear()-1);}') %>%
        dyLegend(labelsDiv = "powellStandardGraphXtsLegend")
    }
    else {
      data <- getResXtsData('powell','new')
      graph <- dygraph(data, main = "Lake Powell EOCY Elevation Percentiles") %>%
        dySeries("10%", label = "10th", strokeWidth = 3, strokePattern = "dotted", color = "#244A9F") %>%
        dySeries("50%", label = "50th", strokeWidth = 3, color = "#244A9F") %>%
        dySeries("90%", label = "90th", strokeWidth = 3, strokePattern = "dashed", color = "#244A9F") %>%
        dyOptions(drawGrid = TRUE) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1.0) %>%
        dyAxis(name="y", label="Lake Elevation (feet above MSL)") %>%
        dyAxis(name="x", label="Year", 
               valueFormatter = 'function(d){date = new Date(d); return date.getFullYear();}', 
               axisLabelFormatter = 'function(d){date = new Date(d); return (date.getFullYear()-1);}') %>%
        dyLegend(labelsDiv = "powellStandardGraphXtsLegend")
    }
  })
  output$surpshortStandardGraphXts <- renderDygraph({
    if(input$surpShortStandardCompare){
      data <- getSurpShortXtsData('combined')
      graph <- dygraph(data, main = "Lower Basin Shortages by Tier") %>%
        dySeries("shortPctg", label = paste(newRunName(), "Shortage",sep=" "), strokeWidth = 3, color = "#244A9F") %>%
        dySeries("surpPctg", label = paste(newRunName(), "Surplus",sep=" "), strokeWidth = 3, color = "#CB9F5B") %>%
        dySeries("short1Pctg", label = paste(newRunName(), "Tier 1 Shortage",sep=" "), strokeWidth = 2, color = "#152C5F") %>%
        dySeries("short2Pctg", label = paste(newRunName(), "Tier 2 Shortage",sep=" "), strokeWidth = 2, color = "#244A9F") %>%
        dySeries("short3Pctg", label = paste(newRunName(), "Tier 3 Shortage",sep=" "), strokeWidth = 2, color = "#6580BB") %>%
        dySeries("shortPctg.1", label = paste(oldRunName(), "Shortage",sep=" "), strokeWidth = 2, strokePattern = "dashed", color = "#244A9F") %>%
        dySeries("surpPctg.1", label = paste(oldRunName(), "Surplus",sep=" "), strokeWidth = 2, strokePattern = "dashed", color = "#CB9F5B") %>%
        dySeries("short1Pctg.1", label = paste(oldRunName(), "Tier 1 Shortage",sep=" "), strokeWidth = 1, strokePattern = "dashed", color = "#152C5F") %>%
        dySeries("short2Pctg.1", label = paste(oldRunName(), "Tier 2 Shortage",sep=" "), strokeWidth = 1, strokePattern = "dashed", color = "#244A9F") %>%
        dySeries("short3Pctg.1", label = paste(oldRunName(), "Tier 3 Shortage",sep=" "), strokeWidth = 1, strokePattern = "dashed", color = "#6580BB") %>%
        dyOptions(drawGrid = TRUE) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.25) %>%
        dyAxis(name="y", label="Percent of traces (%)") %>%
        dyAxis(name="x", label="Year", 
               valueFormatter = 'function(d){ date = new Date(d); return date.getFullYear(); }', 
               axisLabelFormatter = 'function(d){ date = new Date(d); return (date.getFullYear() - 1);}') %>%
        dyLegend(labelsDiv = "surpshortStandardGraphXtsLegend")
    }
    else {
      data <- getSurpShortXtsData('new')
      graph <- dygraph(data, main = "Lower Basin Shortages by Tier") %>%
        dySeries("shortPctg", label = "Shortage", strokeWidth = 3, color = "#244A9F") %>%
        dySeries("surpPctg", label = "Surplus", strokeWidth = 3, color = "#CB9F5B") %>%
        dySeries("short1Pctg", label = "Tier 1 Shortage", strokeWidth = 2, color = "#152C5F") %>%
        dySeries("short2Pctg", label = "Tier 2 Shortage", strokeWidth = 2, color = "#244A9F") %>%
        dySeries("short3Pctg", label = "Tier 3 Shortage", strokeWidth = 2, color = "#6580BB") %>%
        dyOptions(drawGrid = TRUE) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1.0) %>%
        dyAxis(name="y", label="Percent of traces (%)") %>%
        dyAxis(name="x", label="Year", 
               valueFormatter = 'function(d){ date = new Date(d); return date.getFullYear(); }', 
               axisLabelFormatter = 'function(d){ date = new Date(d); return (date.getFullYear() - 1);}') %>%
        dyLegend(labelsDiv = "surpshortStandardGraphXtsLegend")
    }
  })
  output$elevsStandardGraphXts <- renderDygraph({
    if(input$elevsStandardCompare){
      data <- getElevXtsData('combined')
      graph <- dygraph(data, main = "Reservoir Elevation Exceedance") %>%
        dySeries("mead1075ElevExc", label = paste(newRunName(), "Mead < 1,075 in any month",sep=" "), strokeWidth = 3, color = "#244A9F") %>%
        dySeries("mead1025ElevExc", label = paste(newRunName(), "Mead < 1,025 in any month",sep=" "), strokeWidth = 3, color = "#152C5F") %>%
        dySeries("mead1000ElevExc", label = paste(newRunName(), "Mead < 1,000 in any month",sep=" "), strokeWidth = 3, color = "#244A9F") %>%
        dySeries("powl3490ElevExc", label = paste(newRunName(), "Powell < 3,490 in any month",sep=" "), strokeWidth = 3, color = "#CB9F5B") %>%
        dySeries("mead1075ElevExc.1", label = paste(oldRunName(), "Mead < 1,075 in any month",sep=" "), strokeWidth = 1, strokePattern = "dashed", color = "#244A9F") %>%
        dySeries("mead1025ElevExc.1", label = paste(oldRunName(), "Mead < 1,025 in any month",sep=" "), strokeWidth = 1, strokePattern = "dashed", color = "#152C5F") %>%
        dySeries("mead1000ElevExc.1", label = paste(oldRunName(), "Mead < 1,000 in any month",sep=" "), strokeWidth = 1, strokePattern = "dashed", color = "#244A9F") %>%
        dySeries("powl3490ElevExc.1", label = paste(oldRunName(), "Powell < 3,490 in any month",sep=" "), strokeWidth = 1, strokePattern = "dashed", color = "#CB9F5B") %>%
        dyOptions(drawGrid = TRUE) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.25) %>%
        dyAxis(name="y", label="Percent of traces (%)") %>%
        dyAxis(name="x", label="Year", 
               valueFormatter = 'function(d){ date = new Date(d); return date.getFullYear(); }', 
               axisLabelFormatter = 'function(d){ date = new Date(d); return (date.getFullYear() - 1);}') %>%
        dyLegend(labelsDiv = "elevsStandardGraphXtsLegend")
    }
    else {
      data <- getElevXtsData('new')
      graph <- dygraph(data, main = "Reservoir Elevation Exceedance") %>%
        dySeries("mead1075ElevExc", label = "Mead < 1,075 in any month", strokeWidth = 3, color = "#244A9F") %>%
        dySeries("mead1025ElevExc", label = "Mead < 1,025 in any month", strokeWidth = 3, color = "#152C5F") %>%
        dySeries("mead1000ElevExc", label = "Mead < 1,000 in any month", strokeWidth = 3, color = "#244A9F") %>%
        dySeries("powl3490ElevExc", label = "Powell < 3,490 in any month", strokeWidth = 3, color = "#CB9F5B") %>%
        dyOptions(drawGrid = TRUE) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1.0) %>%
        dyAxis(name="y", label="Percent of traces (%)") %>%
        dyAxis(name="x", label="Year", 
               valueFormatter = 'function(d){ date = new Date(d); return date.getFullYear(); }', 
               axisLabelFormatter = 'function(d){ date = new Date(d); return (date.getFullYear() - 1);}') %>%
        dyLegend(labelsDiv = "elevsStandardGraphXtsLegend")
    }
  })
# GENERATE DOWNLOAD DATA BUTTON ON THE REPORTS TAB
  output$downloadMeadStandardData <- downloadHandler(
    filename = function() 
    {paste('temp',Sys.time(),'.csv', sep='')},
    content = function(filename) {
      if(input$meadStandardCompare){
        data <- getResXtsData('mead','combined')
        names(data) <- c(paste(newRunName(),c("-10th","-50th","-90th"),sep=""),paste(oldRunName(),c("-10th","-50th","-90th"),sep=""))
        write.csv(data.frame(Date=index(data),coredata(data)), filename,row.names = FALSE) 
      }
      else {
        data <- getResXtsData('mead','new')
        names(data) <- c(paste(newRunName(),c("-10th","-50th","-90th"),sep=""))
        write.csv(data.frame(Date=index(data),coredata(data)), filename,row.names = FALSE) 
      }
    }
  )
  output$downloadPowellStandardData <- downloadHandler(
    filename = function() 
    {paste('temp',Sys.time(),'.csv', sep='')},
    content = function(filename) {
      if(input$powellStandardCompare){
        data <- getResXtsData('powell','combined')
        names(data) <- c(paste(newRunName(),c("-10th","-50th","-90th"),sep=""),paste(oldRunName(),c("-10th","-50th","-90th"),sep=""))
        write.csv(data.frame(Date=index(data),coredata(data)), filename,row.names = FALSE) 
      }
      else {
        data <- getResXtsData('powell','new')
        names(data) <- c(paste(newRunName(),c("-10th","-50th","-90th"),sep=""))
        write.csv(data.frame(Date=index(data),coredata(data)), filename,row.names = FALSE) 
      }
    }
  )
  output$downloadSrShortStandardData <- downloadHandler(
    filename = function() 
    {paste('temp',Sys.time(),'.csv', sep='')},
    content = function(filename){
      if(input$surpShortStandardCompare){
        data <- getSurpShortXtsData('combined')
        names(data) <- c(paste(newRunName(),c("-Shortage","-Surplus","-Tier1 Shortage","-Tier2 Shortage","-Tier3 Shortage"),sep=""),
                         paste(oldRunName(),c("-Shortage","-Surplus","-Tier1 Shortage","-Tier2 Shortage","-Tier3 Shortage"),sep=""))
        write.csv(data.frame(Date=index(data),coredata(data)), filename,row.names = FALSE) 
      }
      else {
        data <- getSurpShortXtsData('new')
        names(data) <- c(paste(newRunName(),c("-Shortage","-Surplus","-Tier1 Shortage","-Tier2 Shortage","-Tier3 Shortage"),sep=""))
        write.csv(data.frame(Date=index(data),coredata(data)), filename,row.names = FALSE) 
      }      
    }
  )
  output$downloadElevStandardData <- downloadHandler(
    filename = function() 
    {paste('temp',Sys.time(),'.csv', sep='')},
    content = function(filename) {
      if(input$elevsStandardCompare){
        data <- getElevXtsData('combined')
        names(data) <- c(paste(newRunName(),c("-Mead LT 1075","-Mead LT 1025","-Mead LT 1000", "-Powell LT 3490"),sep=""),
                         paste(oldRunName(),c("-Mead LT 1075","-Mead LT 1025","-Mead LT 1000", "-Powell LT 3490"),sep=""))
        write.csv(data.frame(Date=index(data),coredata(data)), filename,row.names = FALSE) 
      }
      else {
        data <- getElevXtsData('new')
        names(data) <- c(paste(newRunName(),c("-Mead LT 1075","-Mead LT 1025","-Mead LT 1000", "-Powell LT 3490"),sep=""))
        write.csv(data.frame(Date=index(data),coredata(data)), filename,row.names = FALSE) 
      }      
    }
  )
}