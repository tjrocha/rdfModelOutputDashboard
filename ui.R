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
      ),
    dropdownMenuOutput("rdfInfoMenu"),
    dropdownMenuOutput("slotInfoMenu")
    )
  ),
  # DASHBOARD SIDEBAR
  dashboardSidebar
  (
    # DEFINE SIDEBAR ITEMS
    uiOutput("selectModelName"),
    uiOutput("resettableFileInput"),
    uiOutput("selectSlotName"),
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
        "1. Select a model from the top-most drop down box or click on 'Choose File' to select and open ",
        "an RDF file. The current file size limit is 30MB while in the beta testing phase.",
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
      ),
      fluidRow #[JR] PLOTS ARE MAPPED TO A PLOT IN THE server SECTION BELOW
      (
        box
        (
          dygraphOutput("plotRdfThreshCheck"),
          br(),
          "Note: Click-and-drag to zoom in. Double-click to undo. Select a data aggregation scheme ",
          "and a threshold to compare the data against",
          radioButtons("threshDataType", label = "Select aggregation: ", 
                       c("Raw Data" = "none", "EOCY Value" = "eocy", "CY Sum"="cysum"),
                       selected = "none",inline = TRUE),
          radioButtons("threshCompType", label = "Select threshold comparison: ", 
                       c("Greater Than" = "GT", "Less Than"="LT"),
                       selected = "LT",inline = TRUE),
          textInput("threshValue", "Input Value(s) Example: 1075,1050,1025", "0")
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