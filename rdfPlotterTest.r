##############################################################################
# This codefile used to automate the CRSS processing of model outputs 
#   
#
#
# Jon Rocha ~ jrocha@usbr.gov ~ (702) 293-8379
##############################################################################

##############################################################################
# SETUP & INITIALIZATION
##############################################################################

# SET WORKING DIRECTORY & CLEANUP R WORKSPACE
rm(list=ls())
setwd("~/R/RProjects/TWS.DataPlotter")

# LOAD REQUIRED LIBRARIES
suppressWarnings(library(RWDataPlot))
suppressWarnings(library(zoo))
suppressWarnings(library(xts))
suppressWarnings(library(dygraphs))

# DEFINE AND READ RDF FILE FOR PROCESSING
rdfFileName <- 'MTOM.rdf' #'TWS_DNFcurrent.rdf'
rdf <- read.rdf(rdfFileName)
jrDebug = TRUE #[JR] testing and manual processing
debugIthSlot = 19#[JR] testing and manual processing

# GET THE TIME COMPONENT FROM RDF FOR XTS
tArray <- rdf$runs[[1]]$times

##############################################################################
# DEFINE FUNCTIONS
##############################################################################

# FUNCTION TO PERFORM STATS, AGGREGATION AND ANALYSIS
getStats = function(procData)
{
  
  # DEFINE PERCENTILE VALUES OF INTEREST
  toPctls <- function(procData) quantile(procData, c(.10,.25,.50,.75,.90))
  # GET PERCENTILE VALUES OF ENTIRE ARRAY BY EOCY
  eocyPctlXts <- apply.yearly(procData[endpoints(procData, on="years", k=1)],toPctls)
  # GET PERCENTILE VALUES OF ENTIRE ARRAY BY MONTH
  montPctlXts <- apply.monthly(procData,toPctls)
  # GET CY ANNUAL SUMS BY TRACE 
  annlSumsXts <- apply.yearly(procData,mean)*12
  # GET PERCENTILE VALUES OF ENTIRE ARRAY BY CY ANNUAL SUMS 
  annlSumsPctlXts <- apply.yearly(annlSumsXts,toPctls)
  # SORT EACH COLUMN DESCENDING FOR MONTHLY VALUE CDF PLOTS
  sortedDataXts <- apply(procData,2,sort,decreasing=TRUE)
  # GET PERCENTILE VALUES OF SORTED DATA AT EACH ROW
  sortedDataPctlsXts <- apply(sortedDataXts, 1, toPctls)
  # FLIP ARRAY FOR PLOTTING
  sortedDataPctlsXts <- t(sortedDataPctlsXts[nrow(sortedDataPctlsXts):1,])
  sortedDataPctlsXts <- data.frame(cbind(round((1000*array((1:nrow(sortedDataPctlsXts))/nrow(sortedDataPctlsXts)))+1000,0),sortedDataPctlsXts))
  sortedDataPctlsXts <- as.xts(sortedDataPctlsXts[-1], order.by = as.Date(paste0(sortedDataPctlsXts$V1,"-01-01",format="%Y-01-01")))
  # PACK XTS ARRAYS INTO A LIST AND OUTPUT
  outList <- list(eocyPctlXts,montPctlXts,annlSumsXts,annlSumsPctlXts,sortedDataPctlsXts)
  return(outList)
}

# FUNCTION TO GENERATE PLOTS
getPlots = function(statValues)
{
  # UNPACK LIST CONTAINING XTS STATS
  eocyPctl <- statValues[[1]]         #EOCY Percentile Values
  montPctl <- statValues[[2]]         #Monthly Percentile Values
  annlSums <- statValues[[3]]         #CY Annual Sums
  annlSumsPctl <- statValues[[4]]     #CY Annual Sums Percentile Values
  sortedDataPctls <- statValues[[5]]  #Sorted Data Percentile for Exceedance Plots
  
  ##############################################################################
  # PLOT INTERACTIVE CDF FOR MONTHLY VALUES
  # FOR ALL
  plotCdfMonthly <- dygraph(sortedDataPctls, main = paste(ithSlotName,"Monthly Statistics"), 
                            ylab = "Value",
                            xlab = "Percent Exceedance") %>%
    dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 1.0,
                hideOnMouseOut = TRUE) %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), strokeWidth = 3, fillAlpha = .4, pointSize = 10) %>%
    dyLegend(show = "always", hideOnMouseOut = TRUE, width = 600) %>%
    dySeries(c("X10."), label = "10th Percentile", strokePattern = "dashed", color = "blue") %>%
    dySeries(c("X90."), label = "90th Percentile", strokePattern = "dashed", color = "red") %>%
    dySeries(c("X25.", "X50.", "X75."), label = "Median", strokeWidth = 7, color = "black") %>%
    dyAxis(name="x" , valueFormatter = "function(d){ date = new Date(d); return (date.getFullYear()-1000)/10; }" , axisLabelFormatter = "function(d){ return Math.round((d.getFullYear()-1000)/10) }" )
  
  ##############################################################################
  # PLOT EOCY TS PERCENTILE
  # FOR RESERVOIR ELEVATIONS
  plotTsEocyPctl <- dygraph(eocyPctl, main = paste(ithSlotName,"EOCY Statistics"), 
                            ylab = "Value",
                            xlab = "Shaded Area is the IQR") %>%
    dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 1.0,
                hideOnMouseOut = TRUE) %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), strokeWidth = 3, fillAlpha = .4, pointSize = 10) %>%
    dyLegend(show = "always", hideOnMouseOut = TRUE, width = 600) %>%
    dyRangeSelector() %>%
    dySeries(c("10%"), label = "10th Percentile", strokePattern = "dashed", color = "blue") %>%
    dySeries(c("90%"), label = "90th Percentile", strokePattern = "dashed", color = "red") %>%
    dySeries(c("25%", "50%", "75%"), label = "Median", strokeWidth = 7, color = "black")
  
  ##############################################################################
  # PLOT CY SUMS TS PERCENTILE
  # FOR FLOWS AND VOLUMES
  plotTsCySumPctl <- dygraph(annlSumsPctl, main = paste(ithSlotName,"CY Sum Statistics"), 
                             ylab = "Value",
                             xlab = "Shaded Area is the IQR") %>%
    dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 1.0,
                hideOnMouseOut = TRUE) %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), strokeWidth = 3, fillAlpha = .4, pointSize = 10) %>%
    dyLegend(show = "always", hideOnMouseOut = TRUE, width = 600) %>%
    dyRangeSelector() %>%
    dySeries(c("10%"), label = "10th Percentile", strokePattern = "dashed", color = "blue") %>%
    dySeries(c("90%"), label = "90th Percentile", strokePattern = "dashed", color = "red") %>%
    dySeries(c("25%", "50%", "75%"), label = "Median", strokeWidth = 7, color = "black")
  
  ##############################################################################
  # PLOT MONTHLY TRACES
  # FOR ALL
  plotTsRawData <- dygraph(procData, main = paste(ithSlotName,"Traces"),
                           ylab = "Value") %>%
    dyOptions(colors = "azure1",strokeWidth = 0.2, strokePattern = "dashed", fillAlpha = .25) %>%
    dyRangeSelector() %>%
    dyLegend(show = "never", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  
  ##############################################################################
  # PLOT STATIC GGPLOT CDF FOR MONTHLY VALUES
  # FOR ALL
  #     plotStaticCdfMonthly <- ggplot(data=data.frame(sortedDataPctls), aes(data[,1])) +
  #       geom_line(aes(y = X10.),linetype="dashed",colour="blue",size=1) +
  #       geom_line(aes(y = X50.),colour="blue",size=1.5) +
  #       geom_line(aes(y = X90.),linetype="dashed",colour="blue",size=1) +
  #       labs(title = paste("Exceedance for the monthly 10-50-90 percentile levels: ",ithSlotName), x = "Exceedance Percentile (%)", y = "Value") + 
  #       theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
  #             plot.title=element_text(size=20,face="bold"))+
  #       xlim(0,100) +
  #       geom_ribbon(data=sortedDataPctls, aes(ymin=X10.,ymax=X90.), fill="blue", alpha="0.2")
  
  # PACK PLOTS INTO A LIST AND OUTPUT
  outList <- list(plotCdfMonthly,plotTsEocyPctl,plotTsCySumPctl,plotTsRawData)
  return(outList)
}

##############################################################################
# MAIN PROCESS
##############################################################################
# THIS LOOPS THROUGH EACH OF THE SLOTS IN THE SELECTED RDF FILE
slotNames = listSlots(rdf)
slotNameCounter = 0
for (ithSlotName in slotNames)
{
  if (jrDebug)
  { ithSlotName = slotNames[debugIthSlot] }
  
  # GET iTH SLOTNAME TO PROCESS
  slotNameCounter <- slotNameCounter + 1
  print(paste('Processing ',slotNameCounter,'of',length(slotNames),': ',ithSlotName))
  
  # DEFINE DATETIME CONVERSION FUNCTION FOR RDF TO XTS
  #toDate <- function(procData) as.POSIXct(strptime(procData, "%y-%m-%d %H:%M:%ss"))
  
  # OPERATIONS IN ORDER OF EXECUTION
  # 1. rdfSlotToMatrix - read data for 'ithSlotName' string given 'rdf' file
  # 2. cbind - combine datetime and data series arrays
  # 3. data.frame - define R dataframe for conversion to XTS
  # 4. read.zoo - convert dataframe to zoo matrix
  # 5. as.xts - convert zoo matrix to XTS
  # 6. Storage.mode() - convert char values in the XTS matrix to numeric
  procData <- as.xts(read.zoo(data.frame(cbind(tArray,rdfSlotToMatrix(rdf, ithSlotName)))))
  storage.mode(procData) <- "numeric"
  
  # CALL FUNCTION TO PERFORM STATS, AGGREGATION, AND ANALYSIS
  statValues <- getStats(procData)
  # statValues CONTENTS:
    #  eocyPctl <- statValues[[1]]         #EOCY Percentile Values
    #  montPctl <- statValues[[2]]         #Monthly Percentile Values
    #  annlSums <- statValues[[3]]         #CY Annual Sums
    #  annlSumsPctl <- statValues[[4]]     #CY Annual Sums Percentile Values
    #  sortedDataPctls <- statValues[[5]]  #Sorted Data Percentile for Exceedance Plots
  
  # CALL FUNCTION TO GENERATE PLOTS USING OUTPUTS FROM THE STATISTICAL ANALYSIS
  plotList <- getPlots(statValues)
  # plotList CONTENTS:  
    #  plotCdfMonthly <- plotList[[1]]      #Monthly Percentile Values
    #  plotTsEocyPctl <- plotList[[2]]      #EOCY Percentile Values
    #  plotTsCySumPctl <- plotList[[3]]     #CY Annual Sums Percentile Values
    #  plotTsRawData <- plotList[[4]]       #Raw Traces

  # ONLY DO THE SPECIFIED SLOT FOR DEBUGGING OR MANUAL RUNS
  if (jrDebug)
  { break }
    
  # SAVE AND OUPUT PLOTS AS HTML FILES
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  origWD <- getwd()
  dir.create(trim(ithSlotName),FALSE)
  setwd(trim(ithSlotName))
  #outList <- list(plotCdfMonthly,plotTsEocyPctl,plotTsCySumPctl,plotTsRawData)
  htmlwidgets::saveWidget(plotList[[1]],'cdfMonthly.html')
  htmlwidgets::saveWidget(plotList[[2]],'tsEocyPctl.html')
  htmlwidgets::saveWidget(plotList[[3]],'tsCySumPctl.html')
  htmlwidgets::saveWidget(plotList[[4]],'tsRawData.html')
  setwd(origWD)
}



