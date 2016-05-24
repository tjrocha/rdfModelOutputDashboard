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
# DEFINE DASHBOARD HEADER
############################################################################################
dbHeader <- dashboardHeader(
  title=""
)
dbHeader$children[[2]]$children <-  tags$a(href='http://www.usbr.gov/',tags$img(src='logo.png',height='38',width='200'))
############################################################################################
# DEFINE DASHBOARD SIDEBAR
############################################################################################
dbSidebar <- dashboardSidebar(
  # DEFINE SIDEBAR ITEMS
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Report", tabName = "report", icon = icon("table")),
    menuItem("Explore", tabName = "explore", icon = icon("area-chart")),
    menuItem("Source Code", icon = icon("code"), href = "https://github.com/tjrocha/RiverWareRdfOutputExplorer")
  )
)
############################################################################################
# DEFINE DASHBOARD BODY HOME TAB
############################################################################################
homeTab <- tabItem(
  tabName = "home",
  h1("CRSS Output Explorer"),
  fluidRow(
    tags$em(
    "BETA VERSION STATEMENT: This data exploration tool is intended for use by U.S. Bureau of Reclamation ",
    "(Reclamation) modelers, Reclamation's stakeholders, academia, and the public. The data presented here ",
    "are outputs from a water resources model. You may request a subset of the data by contacting us directly ",
    "via email. Please send questions, comments, suggestions for improvements, and error reports via email to ",
    "Jon Rocha (jrocha@usbr.gov). The current web location for this tool is temporary and it will be hosted on ",
    "a Reclamation server as soon as a suitable one can be located."
    ),
    br(),br(),
    box(
      h2("Instructions"),
      "1. The typical graphs that are generated for CRSS runs are shown in the Report ",
      icon("table"),
      " tab.",
      br(),
      "2. You may click on the Explore ",
      icon("area-chart"),
      " tab to select specific CRSS runs and outputs to graph, tabulate, and analyze.", 
      br(),br()
    ),
    box(
      h2("Information"),
      "These modeling results were generated based on hydrologic and operations projections for ",
      "the Colorado River system reservoirs. The modeling tool used to generate these results is ",
      tags$a(href="http://www.riverware.org/", "RiverWare"),
      icon("registered"),
      br(),br(),
      "Results contained in the RDF files project future reservoir conditions and potential dam ",
      "operations for the Colorado River system reservoirs given existing reservoir conditions, ",
      "inflow forecasts and projections, inflow assumptions, and a variety of operational policies ",
      "and guidelines. ",
      br(),br(),
      "All future conditions and operations used in the models to generate these results are only ",
      "projections and are subject to change as actual hydrology, operations, and forecasts diverge ",
      "from those used to produce specific model projections. ",
      br(),br(),
      "This dashboard allows users to query, subset, view, and plot RiverWare model outputs. ",
      "This tool is primarily meant to support U.S. Bureau of Reclamation modeling and analysis ",
      "efforts with the Colorado River Simulation ",
      "System models. ",
      br(),br(),
      "The dashboard uses the following R libraries below and is being developed in RStudio. ",
      br(),
      tags$a(href="http://shiny.rstudio.com/", "shiny"),
      ", ",
      tags$a(href="https://rstudio.github.io/shinydashboard", "shinydashboard"),
      ", ",
      tags$a(href="https://github.com/trestletech/shinyTree", "shinyTree"),
      ", ",
      tags$a(href="https://rstudio.github.io/DT", "DT"),
      ", ",
      tags$a(href="https://cran.r-project.org/web/packages/xts/index.html", "xts"),
      ", ",
      tags$a(href="https://cran.r-project.org/web/packages/zoo/index.html", "zoo"),
      ", ",
      tags$a(href="https://github.com/rabutler/RWDataPlot", "RWDataPlot"),
      br(),br(),
      "The source code is available on ",
      tags$a(href="https://github.com/tjrocha/RiverWareRdfOutputExplorer", "GitHub. "),
      "Contact the developer, Jon Rocha, via e-mail at jrocha@usbr.gov for questions or feedback."
    )
  )
)
############################################################################################
# DEFINE DASHBOARD BODY GRAPHS TAB
############################################################################################
exploreTab <- tabItem(
  tabName = "explore",
  h2("Instructions"),
  "1. Select a model run from the drop down box on the left-hand-side.",
  br(),
  "2. Once a model run has been selected, another drop down box will be populated with the slot names ",
  "present in the selected model run. You may click on the drop-down box to specify a slot ",
  "to select or you may type in partial names to filter the available slots in the list. ",
  "The drop-down box may take a few seconds to generate.",
  br(),
  "3. Once a run and a slot has been selected, you may now view graphs and data in the page below. ",
  "Note that it takes ~4 seconds to populate for the typical CRSS run with 107 traces and ",
  "~75 seconds for the occasional run with 3,210 traces",
  br(),br(),
  fluidRow(
    box(
      uiOutput("selectModelName")
      #uiOutput("resettableFileInput")
    ),
    box(
      uiOutput("selectSlotName") 
    )
  ),
  h2("Graphs & Data"),
  "Notes:",
  br(),
  "1. Graphs shown below are based on the selected Run and Slot on the top menu. ",
  br(),
  "2. Some plots have interactive elements while some do not. ",
  br(),br(),
  fluidRow(
    # RAW DATA GRAPH
    dygraphOutput("plotRdfTS"),
    "Note: Click-and-drag to zoom in. Double-click to undo. You may click on the slider and then use your ",
    "keyboard left and right keys to cycle through each trace.",
    htmlOutput("selectedTrace")
  ),
  br(),br(),
  fluidRow(
    # ENVELOPE GRAPH BOX
    box(
      dygraphOutput("plotRdfEnv"),
      br(),
      textOutput("plotRdfEnvLegend"),
      br(),
      "Note: Click-and-drag to zoom in. Double-click to undo. The Interquartile Range or 25-50-75 ",
      "percentile levels are already shown by the shaded range",
      radioButtons("envChartType", label = "Select aggregation: ", 
                   c("EOCY" = "eocy", "Monthly"="monthly","CY Sum"="cysum"),
                   selected = "monthly",inline = TRUE),
      sliderInput("envChartRange", label = "Envelope range: ",min = 0, max = 100, value = c(10, 90)),
      br(),
      downloadButton('downloadEnvelopeAggSelectedData', 'Download Data as a CSV file')
    ), 
    # THRESHOLD GRAPH BOX
    box(
      dygraphOutput("plotRdfThreshCheck"),
      br(),
      textOutput("plotRdfThreshCheckLegend"),
      br(),
      "Note: Click-and-drag to zoom in. Double-click to undo. Select a data aggregation scheme ",
      "and a threshold to compare the data against",
      radioButtons("threshDataType", label = "Select data filter/aggregation: ", 
                   c("Raw Data" = "none", "EOCY Value" = "eocy", "CY Sum"="cysum"),
                   selected = "none",inline = TRUE),
      radioButtons("threshCompType", label = "Select threshold comparison: ", 
                   c("Greater Than" = "GT", "Less Than"="LT"),
                   selected = "LT",inline = TRUE),
      textInput("threshValue", "Input threshold value(s): (Example: 1075,1050,1025)", "0"),
      br(),
      downloadButton('downloadThresoldChartDataData', 'Download Data as a CSV file')
    ) 
  ),
  h2("Data Table"),
  "Notes:",
  br(),
  "1. Graphs shown below are based on the selected Run and Slot on the top menu. ",
  br(),
  "2. Clicking on the Download Data button will save the entire contents of the table as a ",
  "comma-separated-variables (CSV) file on your local machine. ",
  br(),
  "3. Clicking on the column headers will sort your data in ascending/descending order for that particular column. ",
  br(),
  "4. You may type anything in the search textbox to further filter the results. ",
  br(),  
  "Search Function Examples: Typing '2010' or '12-31' will filter the data to just ",
  "those for 2010 or December-31 for all years respectively. ",
  "Typing '1075' will filter data values outside of 1075.00 to 1075.99. ",
  br(),
  "5. Clicking on the empty boxes below each header shows the range of the data in that particular column ",
  "via a slider bar which you may also use to filter the table rows.",
  br(),br(),
  downloadButton('downloadDataTable', 'Download Data as a CSV file'),
  br(),br(),
  fluidRow(
    div(
      style = 'overflow-x: scroll',
      DT::dataTableOutput("tableRdfData") #[JR] tableRdfData IS MAPPED TO A TABLE IN THE server SECTION BELOW
    )
  )
)
############################################################################################
# DEFINE DASHBOARD BODY TABLE TAB
############################################################################################
reportTab <- tabItem(
   tabName = "report",
   h2("Typical CRSS Results"),
   "Notes:",
   br(),
   "1. The slots shown here are based on the typical graphs generated for CRSS outputs. ",
   br(),
   "2. Zoom in by clicking-and-dragging a range within each plot. Double-click to unzoom.",
   br(),
   "3. You may compare results from the most recent CRSS run against the previous run by ",
   "toggling the check box under each plot.",
   br(),
   "4. Click on the Download Data button to donwload the underlying data shown in each plot.",
   br(),br(),
   fluidRow(
     box(
       dygraphOutput("meadStandardGraphXts"),
       br(),
       textOutput("meadStandardGraphXtsLegend"),
       checkboxInput("meadStandardCompare", "Compare with previous run", FALSE),
       downloadButton('downloadMeadStandardData', 'Download Data as a CSV file')
       # downloadButton('downloadMeadChart', 'Download Chart as HTML file')
     ),
     box(
       dygraphOutput("powellStandardGraphXts"),
       br(),
       textOutput("powellStandardGraphXtsLegend"),
       checkboxInput("powellStandardCompare", "Compare with previous run", FALSE),
       downloadButton('downloadPowellStandardData', 'Download Data as a CSV file')
     )
   ),
   fluidRow(
     box(
       dygraphOutput("surpshortStandardGraphXts"),
       br(),
       textOutput("surpshortStandardGraphXtsLegend"),
       checkboxInput("surpShortStandardCompare", "Compare with previous run", FALSE),
       downloadButton('downloadSrShortStandardData', 'Download Data as a CSV file')
     ),
     box(
       dygraphOutput("elevsStandardGraphXts"),
       br(),
       textOutput("elevsStandardGraphXtsLegend"),
       checkboxInput("elevsStandardCompare", "Compare with previous run", FALSE),
       downloadButton('downloadElevStandardData', 'Download Data as a CSV file')
       
     )
   )
)

############################################################################################
# POPULATE DASHBOARD
############################################################################################
userInterface <- dashboardPage(
  skin = "blue",
  # DASHBOARD HEADER
  dbHeader,
  # DASHBOARD SIDEBAR
  dbSidebar,
  # DASHBOARD BODY
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "rdfTool.css")),
    tabItems(
      homeTab,
      reportTab,
      exploreTab
    )
  )
)