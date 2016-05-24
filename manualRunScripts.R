############################################################################################
# This application was built in RStudio by Jon Rocha at jrocha@usbr.gov
#
# Run this script to initialize the data needed by the CRSS Tool. This file needs to be run in 
# the directory that has the following files.
#     1. The most recent MPPE.rdf file named as newMPPE.rdf
#     2. The previous MPPE.rdf file named as oldMPPE.rdf
#     3. The most recent SystemConditions.rdf file named as newSystemConditions.rdf
#     4. The previous SystemConditions.rdf file named as oldSystemConditions.rdf
#     
# The output R-Data-Objects from this script would then be packaged with or uploaded to 
#     the CRSS tool for use.
#     
# Application is distributed with an MIT license, March 2016
############################################################################################

setwd("~/R/RProjects/rdfModelOutputDashboardCRSS_Version")

rm(list=ls())
library(dygraphs)
library(DT)
library(xts)
library(zoo)
library(RWDataPlot)
source('global.R')

# DATA FUNCTIONS
initElevXtsData <- function(mppeData) {
  runName <- format(as.Date(mppeData$meta$create_date),format="%b%Y")
  rdfXTS <- rdfSlotToXTS(mppeData, 'Mead.Pool Elevation')
  mead1075ElevExc <- getArrayThresholdExceedance(getTraceMin(rdfXTS,"CY"), 1075, "LT")[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
  mead1025ElevExc <- getArrayThresholdExceedance(getTraceMin(rdfXTS,"CY"), 1025, "LT")[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
  mead1000ElevExc <- getArrayThresholdExceedance(getTraceMin(rdfXTS,"CY"), 1000, "LT")[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
  rdfXTS <- rdfSlotToXTS(mppeData, 'Powell.Pool Elevation')
  powl3490ElevExc <- getArrayThresholdExceedance(getTraceMin(rdfXTS,"CY"), 3490, "LT")[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
  
  dTab <- merge(mead1075ElevExc,mead1025ElevExc)
  dTab <- merge(dTab,mead1000ElevExc)
  dTab <- merge(dTab,powl3490ElevExc)
  dTab
}
initResXtsData <- function(mppeData, resName){
  rdfXts <- rdfSlotToXTS(mppeData, resName)
  rdfXts <- getTraceMonthVal(rdfXts,12)
  rdfXts <- getArrayPctl(rdfXts,c(0.1,0.5,0.9))[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
  rdfXts
}
initSurpShortXtsData <- function(sysCondData) {
  #   rdfName <- 'newSystemConditions.rdf'
  #   rawRDF <- read.rdf(rdfName)
  runName <- format(as.Date(sysCondData$meta$create_date),format="%b%Y")
  
  surpPctg <- rdfSlotToXTS(sysCondData, 'SummaryOutputData.LBSurplusConditions')
  surpPctg <- surpPctg[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
  surpPctg <- getArrayThresholdExceedance(surpPctg, 1, 'EQ')
  
  shortPctg <- rdfSlotToXTS(sysCondData, 'SummaryOutputData.LBShortageConditions')
  shortPctg <- shortPctg[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
  shortPctg <- getArrayThresholdExceedance(shortPctg, 1, 'EQ')
  
  short1Pctg <- rdfSlotToXTS(sysCondData, 'SummaryOutputData.LBShortageStep1')
  short1Pctg <- short1Pctg[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
  short1Pctg <- getArrayThresholdExceedance(short1Pctg, 1, 'EQ')
  
  short2Pctg <- rdfSlotToXTS(sysCondData, 'SummaryOutputData.LBShortageStep2')
  short2Pctg <- short2Pctg[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
  short2Pctg <- getArrayThresholdExceedance(short2Pctg, 1, 'EQ')
  
  short3Pctg <- rdfSlotToXTS(sysCondData, 'SummaryOutputData.LBShortageStep3')
  short3Pctg <- short3Pctg[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
  short3Pctg <- getArrayThresholdExceedance(short3Pctg, 1, 'EQ')
  
  dTab <- merge(shortPctg,surpPctg)
  dTab <- merge(dTab,short1Pctg)
  dTab <- merge(dTab,short2Pctg)
  dTab <- merge(dTab,short3Pctg)
  dTab
}

# DATA INITIALIZATION
newMppeData <- read.rdf('newMPPE.rdf')
save(newMppeData,file='newMPPE.rds')
oldMppeData <- read.rdf('oldMPPE.rdf')
save(oldMppeData,file='oldMPPE.rds')
newSysCondData <- read.rdf('newSystemConditions.rdf')
save(newSysCondData,file='newSystemConditions.rds')
oldSysCondData <- read.rdf('oldSystemConditions.rdf')
save(oldSysCondData,file='oldSystemConditions.rds')

a <- initElevXtsData(newMppeData)[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
c <- initSurpShortXtsData(newSysCondData)[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
e <- initResXtsData(newMppeData, 'Mead.Pool Elevation')[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
g <- initResXtsData(newMppeData, 'Powell.Pool Elevation')[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]

minYear <- format(min(index(a)),format="%Y")

b <- initElevXtsData(oldMppeData)[paste(minYear,"/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
d <- initSurpShortXtsData(oldSysCondData)[paste(minYear,"/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
f <- initResXtsData(oldMppeData, 'Mead.Pool Elevation')[paste(minYear,"/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
h <- initResXtsData(oldMppeData, 'Powell.Pool Elevation')[paste(minYear,"/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]

standardResPlotData <- list()
standardResPlotData[[1]] <- a
standardResPlotData[[2]] <- b
save(standardResPlotData,file='standardResPlotData.rds')

standardSurpShortPlotData <- list()
standardSurpShortPlotData[[1]] <- c
standardSurpShortPlotData[[2]] <- d
save(standardSurpShortPlotData,file='standardSurpShortPlotData.rds')

standardMeadPlotData <- list()
standardMeadPlotData[[1]] <- e
standardMeadPlotData[[2]] <- f
save(standardMeadPlotData,file='standardMeadPlotData.rds')

standardPowellPlotData <- list()
standardPowellPlotData[[1]] <- g
standardPowellPlotData[[2]] <- h
save(standardPowellPlotData,file='standardPowellPlotData.rds')

newRunName <- function(rdfXTS)
{
  return(format(as.Date(newMppeData$meta$create_date),format="%b%Y"))
}

# # FUNCTION TO BUILD THE RESERVOIR ELEVATION EXCEEDANCE PLOTS
# buildResChart <- function (rawRDF, slotName, maxYear, compareOld2New) {
#   usbrBlue <- c("#152C5F", "#244A9F", "#6580BB")
#   usbrSand <- c("#8E6F3F", "#CB9F5B", "#DABB8C")
#   runName <- format(as.Date(rawRDF$meta$create_date),format="%b%Y")
#   rdfXTS <- rdfSlotToXTS(rawRDF, slotName)
#   rdfXtsDecVal <- getTraceMonthVal(rdfXTS,12)
#   rdfXtsDecPctl <- getArrayPctl(rdfXtsDecVal,c(0.1,0.5,0.9))[paste("/",maxYear,sep="")]
#   ggData <- data.frame(Date=index(rdfXtsDecPctl),coredata(rdfXtsDecPctl))
#   ggChartResPctls <- ggplot(ggData, aes(x=Date)) + 
#     geom_line(aes(y = X10., colour=paste(runName, "90th",sep=":")), size = 2, linetype = 3) + 
#     geom_line(aes(y = X50., colour=paste(runName, "50th",sep=":")), size = 2, linetype = 1) + 
#     geom_line(aes(y = X90., colour=paste(runName, "10th",sep=":")), size = 2, linetype = 2) +
#     scale_colour_manual("", breaks = c(paste(runName, "10th",sep=":"), paste(runName, "50th",sep=":"), paste(runName, "90th",sep=":")), 
#                         values = usbrBlue) +
#     labs(title = paste(strsplit(slotName,"\\.")[[1]][1], " EOCY Elevation Exceedance Percentiles",sep = ""), 
#          x = "Year", y = "Lake Elevation (feet above MSL)") +
#     theme(text = element_text(size=20), panel.background = element_rect(fill = "white"), 
#           panel.grid.major = element_line(colour = "gainsboro"),
#           panel.border = element_rect(colour = "gainsboro", fill=NA)) 
#   ggChartResPctls
#   
#   if (compareOld2New){
#     # PROCESS RDF2
#     rdfName2 <- 'oldMPPE.rdf'
#     rawRDF2 <- read.rdf(rdfName2)
#     runName2 <- format(as.Date(rawRDF2$meta$create_date),format="%b%Y")
#     rdfXTS2 <- rdfSlotToXTS(rawRDF2, slotName)
#     minYear <- format(min(index(rdfXtsDecVal)),format="%Y")
#     rdfXtsDecVal2 <- getTraceMonthVal(rdfXTS2,12)
#     rdfXtsDecPctl2 <- getArrayPctl(rdfXtsDecVal2,c(0.1,0.5,0.9))[paste(minYear,"/",maxYear,sep="")]
#     ggData2 <- data.frame(Date=index(rdfXtsDecPctl2),coredata(rdfXtsDecPctl2))
#     ggChartResPctls <- ggChartResPctls + 
#       geom_line(aes(x = ggData2$Date, y = ggData2$X10., colour=paste(runName2, "90th",sep=":")), size = 2, linetype = 3) + 
#       geom_line(aes(x = ggData2$Date, y = ggData2$X50., colour=paste(runName2, "50th",sep=":")), size = 2, linetype = 1) + 
#       geom_line(aes(x = ggData2$Date, y = ggData2$X90., colour=paste(runName2, "10th",sep=":")), size = 2, linetype = 2) +
#       scale_colour_manual("", breaks = c(paste(runName, "10th",sep=":"), paste(runName, "50th",sep=":"), paste(runName, "90th",sep=":"),
#                                          paste(runName2, "10th",sep=":"), paste(runName2, "50th",sep=":"), paste(runName2, "90th",sep=":")),  
#                           values =c(usbrBlue, usbrSand))
#   }
#   ggChartResPctls
# }
# 
# ##########################################################################################
# # THIS IS FOR THE POWELL & MEAD EOCY EXCEEDANCE PLOTS
# 
# maxYear <- "2030"
# # PROCESS RDF1
# rdfName <- 'newMPPE.rdf'
# rawRDF <- read.rdf(rdfName)
# #runName <- format(as.Date(rawRDF$meta$create_date),format="%b%Y")
# #rdfXTS <- rdfSlotToXTS(rawRDF, slotNameMead)
# 
# meadChart <- buildResChart(rawRDF, 'Mead.Pool Elevation', FALSE)
# powellChart <- buildResChart(rawRDF, 'Powell.Pool Elevation', TRUE)
# 
# 
# ##########################################################################################
# # THIS IS FOR THE SURPLUS SHORTAGE PERCENT EXCEEDANCE PLOTS
# rdfName <- 'newSystemConditions.rdf'
# rawRDF <- read.rdf(rdfName)
# runName <- format(as.Date(rawRDF$meta$create_date),format="%b%Y")
# 
# # slotNameShort <- 'SummaryOutputData.LBShortageConditions'
# # rdfXTSshort <- rdfSlotToXTS(rawRDF, slotNameShort)
# # rdfXTSshort <- rdfXTSshort[paste("/",maxYear,sep="")]
# # shortPctg <- getArrayThresholdExceedance(rdfXTSshort, 1, 'EQ')
# # shortPctg <- data.frame(Date=index(shortPctg),coredata(shortPctg))
# 
# slotNameSurp <- 'SummaryOutputData.LBSurplusConditions'
# rdfXTSsurp <- rdfSlotToXTS(rawRDF, slotNameSurp)
# rdfXTSsurp <- rdfXTSsurp[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
# surpPctg <- getArrayThresholdExceedance(rdfXTSsurp, 1, 'EQ')
# surpPctg <- data.frame(Date=index(surpPctg),coredata(surpPctg))
# 
# # ggChartSurpShort <- ggplot() + 
# #   geom_line(aes(x = shortPctg$Date, y = shortPctg$coredata.shortPctg., colour="Shortage"), size = 2, linetype = 1) + 
# #   geom_line(aes(x = surpPctg$Date, y = surpPctg$coredata.surpPctg., colour="Surplus"), size = 2, linetype = 1) + 
# #   scale_colour_manual("", breaks = c("Shortage","Surplus"), values =c("#CB9F5B","#244A9F")) + 
# #   ggtitle(bquote(atop("Lower Basin Surplus or Shortage", atop(.(paste(runName, " CRSS Run",sep="")), "")))) +
# #   labs(x = "Year", y = "Percent of traces (%)") +
# #   theme(text = element_text(size=20), panel.background = element_rect(fill = "white"), 
# #         panel.grid.major = element_line(colour = "gainsboro"),
# #         panel.border = element_rect(colour = "gainsboro", fill=NA)) 
# # ggChartSurpShort
# 
# 
# slotNameShort1 <- 'SummaryOutputData.LBShortageStep1'
# rdfXTSshort1 <- rdfSlotToXTS(rawRDF, slotNameShort1)
# rdfXTSshort1 <- rdfXTSshort1[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
# short1Pctg <- getArrayThresholdExceedance(rdfXTSshort1, 1, 'EQ')
# 
# slotNameShort2 <- 'SummaryOutputData.LBShortageStep2'
# rdfXTSshort2 <- rdfSlotToXTS(rawRDF, slotNameShort2)
# rdfXTSshort2 <- rdfXTSshort2[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
# short2Pctg <- getArrayThresholdExceedance(rdfXTSshort2, 1, 'EQ')
# short2Pctg <- short2Pctg + short1Pctg
# 
# slotNameShort3 <- 'SummaryOutputData.LBShortageStep3'
# rdfXTSshort3 <- rdfSlotToXTS(rawRDF, slotNameShort3)
# rdfXTSshort3 <- rdfXTSshort3[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
# short3Pctg <- getArrayThresholdExceedance(rdfXTSshort3, 1, 'EQ')
# short3Pctg <- short3Pctg + short2Pctg
# 
# short1Pctg <- data.frame(Date=index(short1Pctg),coredata(short1Pctg))
# short2Pctg <- data.frame(Date=index(short2Pctg),coredata(short2Pctg))
# short3Pctg <- data.frame(Date=index(short3Pctg),coredata(short3Pctg))
# 
# ggChartShort <- ggplot() + 
#   geom_area(aes(x = short3Pctg$Date, y = short3Pctg$coredata.short3Pctg., colour="Step 3 Shortage"), size = 1.5, fill="#6580BB", alpha=0.125) + 
#   geom_area(aes(x = short2Pctg$Date, y = short2Pctg$coredata.short2Pctg., colour="Step 2 Shortage"), size = 1.5, fill="#244A9F", alpha=0.250) + 
#   geom_area(aes(x = short1Pctg$Date, y = short1Pctg$coredata.short1Pctg., colour="Step 1 Shortage"), size = 1.5, fill="#152C5F", alpha=0.500) +
#   geom_line(aes(x = surpPctg$Date, y = surpPctg$coredata.surpPctg., colour="Surplus"), size = 2, linetype = 1) +
#   scale_x_date(labels = date_format("%Y")) + 
#   #geom_line(aes(x = short1Pctg$Date, y = short1Pctg$coredata.short1Pctg., colour="Step 1 Shortage"), size = 2, linetype = 1) + 
#   #geom_line(aes(x = short2Pctg$Date, y = short2Pctg$coredata.short2Pctg., colour="Step 2 Shortage"), size = 2, linetype = 1) + 
#   #geom_line(aes(x = short3Pctg$Date, y = short3Pctg$coredata.short3Pctg., colour="Step 3 Shortage"), size = 2, linetype = 1) + 
#   #geom_area(colour="black", fill="blue", alpha=.2)geom_area() + 
#   scale_colour_manual("", breaks = c("Step 1 Shortage","Step 2 Shortage","Step 3 Shortage", "Surplus"), values =c("#152C5F","#244A9F","#6580BB", "#CB9F5B")) + 
#   ggtitle(bquote(atop("Lower Basin Shortages by Tier", atop(.(paste(runName, " CRSS Run",sep="")), "")))) +
#   labs(x = "Year", y = "Percent of traces (%)") +
#   theme(text = element_text(size=20), panel.background = element_rect(fill = "white"), 
#         panel.grid.major = element_line(colour = "gainsboro"),
#         panel.border = element_rect(colour = "gainsboro", fill=NA)) 
# ggChartShort
# 
# 
# ##########################################################################################
# # THIS IS FOR THE ELEVATION THRESHOLD PLOTS
# rdfName <- 'newMPPE.rdf'
# rawRDF <- read.rdf(rdfName)
# runName <- format(as.Date(rawRDF$meta$create_date),format="%b%Y")
# rdfXTS <- rdfSlotToXTS(rawRDF, 'Mead.Pool Elevation')
# mead1075ElevExc <- getArrayThresholdExceedance(getTraceMonthVal(rdfXTS,12), 1075, "LT")[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
# mead1075ElevExc <- data.frame(Date=index(mead1075ElevExc),coredata(mead1075ElevExc))
# mead1025ElevExc <- getArrayThresholdExceedance(getTraceMin(rdfXTS,"CY"), 1025, "LT")[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
# mead1025ElevExc <- data.frame(Date=index(mead1025ElevExc),coredata(mead1025ElevExc))
# mead1000ElevExc <- getArrayThresholdExceedance(getTraceMin(rdfXTS,"CY"), 1000, "LT")[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
# mead1000ElevExc <- data.frame(Date=index(mead1000ElevExc),coredata(mead1000ElevExc))
# rdfXTS <- rdfSlotToXTS(rawRDF, 'Powell.Pool Elevation')
# powl3490ElevExc <- getArrayThresholdExceedance(getTraceMin(rdfXTS,"CY"), 3490, "LT")[paste("/",as.numeric(format(Sys.Date(), "%Y")) + 10,sep="")]
# powl3490ElevExc <- data.frame(Date=index(powl3490ElevExc),coredata(powl3490ElevExc))
# 
# ggChartElevThresh <- ggplot() + 
#   geom_line(aes(x = mead1075ElevExc$Date, y = mead1075ElevExc$coredata.mead1075ElevExc., colour="LB Shortage"), size = 1.5, linetype = 1) + 
#   geom_line(aes(x = mead1025ElevExc$Date, y = mead1025ElevExc$coredata.mead1025ElevExc., colour="Mead < 1,025'"), size = 1.5, linetype = 1) + 
#   geom_line(aes(x = mead1000ElevExc$Date, y = mead1000ElevExc$coredata.mead1000ElevExc., colour="Mead < 1,000'"), size = 1.5, linetype = 1) + 
#   geom_line(aes(x = powl3490ElevExc$Date, y = powl3490ElevExc$coredata.powl3490ElevExc., colour="Powell < 3,490'"), size = 1.5, linetype = 1) + 
#   scale_colour_manual("", breaks = c("LB Shortage","Mead < 1,025'","Mead < 1,000'","Powell < 3,490'"), values =c("#244A9F","#152C5F","#6580BB","#CB9F5B")) + 
#   ggtitle(bquote(atop("Reservoir Elevation Exceedance", atop(.(paste(runName, " CRSS Run",sep="")), "")))) +
#   labs(x = "Year", y = "Percent of traces (%)") +
#   theme(text = element_text(size=20), panel.background = element_rect(fill = "white"), 
#         panel.grid.major = element_line(colour = "gainsboro"),
#         panel.border = element_rect(colour = "gainsboro", fill=NA)) 
# ggChartElevThresh
# 
# 
# 
