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
library(shinyTree)
library(dygraphs)
library(DT)
library(xts)
library(zoo)
library(RWDataPlot)
############################################################################################
# DEFINE DASHBOARD HEADER
############################################################################################
dbHeader <- dashboardHeader(
  title="", 
  dropdownMenuOutput("rdfInfoMenu"),
  dropdownMenuOutput("slotInfoMenu")
)
dbHeader$children[[2]]$children <-  tags$a(href='http://www.usbr.gov/',tags$img(src='logo.png',height='38',width='200'))
############################################################################################
# DEFINE DASHBOARD SIDEBAR
############################################################################################
dbSidebar <- dashboardSidebar(
  # DEFINE SIDEBAR ITEMS
  uiOutput("selectModelName"),
  uiOutput("resettableFileInput"),
  uiOutput("selectSlotName"),
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Graphs", tabName = "graphs", icon = icon("area-chart")),
    menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem("RDF Tree", tabName = "tree", icon = icon("tree")),
    menuItem("Reports", tabName = "reports", icon = icon("cogs")),
    menuItem("Source Code (GitHub Link)", icon = icon("file-code-o"), href = "https://github.com/tjrocha/rdfModelOutputDashboard")
  )
)
############################################################################################
# DEFINE DASHBOARD BODY HOME TAB
############################################################################################
homeTab <- tabItem(
  tabName = "home",
  h1("RiverWare RDF Output Explorer"),
  fluidRow(
    box(
      h2("Instructions"),
      "1. Select a model from the top-most drop down box or click on 'Choose File' to select and open ",
      "an RDF file. The current file size limit is 30MB while in the beta testing phase.",
      br(),br(),
      "2. Once a model has been selected, another drop down box will be populated with the slot names ",
      "present in the selected model RDF file. You may click on the drop-down box to specify a slot ",
      "to select or you may type in partial names to filter the available slots in the list. ",
      "The drop-down box may take a few seconds to generate.",
      br(),br(),
      "3. Once a model and a slot has been selected, you may now view graphs and data in their respective ",
      "sections via the sidebar. Information about your selected RDF file and slot are shown by clicking ",
      "on the icons at the top right of the window. You may change your selections at any time. ",
      br(),br()
    ),
    box(
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
      tags$a(href="http://shiny.rstudio.com/", "shiny"),
      br(),
      tags$a(href="https://rstudio.github.io/shinydashboard", "shinydashboard"),
      br(),
      tags$a(href="https://github.com/trestletech/shinyTree", "shinyTree"),
      br(),
      tags$a(href="https://rstudio.github.io/DT", "DT"),
      br(),
      tags$a(href="https://cran.r-project.org/web/packages/xts/index.html", "xts"),
      br(),
      tags$a(href="https://cran.r-project.org/web/packages/zoo/index.html", "zoo"),
      br(),
      tags$a(href="https://github.com/rabutler/RWDataPlot", "RWDataPlot"),
      br(),br(),
      "The source code is also available on GitHub <https://github.com/tjrocha/rdfModelOutputDashboard>"
    )
  )
)
############################################################################################
# DEFINE DASHBOARD BODY GRAPHS TAB
############################################################################################
graphTab <- tabItem(
  tabName = "graphs",
  h2("Graphs"),
  "Notes:",
  br(),
  "1. Graphs shown below are based on the selected model and slot on the sidebar menu. ",
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
      "Note: Click-and-drag to zoom in. Double-click to undo. Do not select the 25-50-75 ",
      "percentiles on the slider, these are already shown by the shaded range",
      radioButtons("envChartType", label = "Select aggregation: ", 
                   c("EOCY" = "eocy", "Monthly"="monthly","CY Sum"="cysum"),
                   selected = "monthly",inline = TRUE),
      sliderInput("envChartRange", label = "Envelope range: ",min = 0, max = 100, value = c(10, 90))
    ), 
    # CDF GRAPH BOX
    box(
      dygraphOutput("plotRdfCDF"),
      br(),
      "Note: Click-and-drag to zoom in. Double-click to undo. Do not select the 25-50-75 ",
      "percentiles on the slider, these are already shown by the shaded range",
      sliderInput
      ("excChartRange", label = "Envelope range: ",min = 0, max = 100, value = c(10, 90))
    ) 
  ),
  fluidRow(
    # THRESHOLD GRAPH BOX
    box(
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
)
############################################################################################
# DEFINE DASHBOARD BODY TABLE TAB
############################################################################################
tableTab <- tabItem(
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
  fluidRow(
    div(
      style = 'overflow-x: scroll',
      DT::dataTableOutput("tableRdfData") #[JR] tableRdfData IS MAPPED TO A TABLE IN THE server SECTION BELOW
    )
  )
)
############################################################################################
# DEFINE DASHBOARD BODY RDF TREE TAB
############################################################################################
treeTab <- tabItem(
  tabName = "tree",
  h2("RDF Tree"),
  fluidRow(
    box(
      "Notes:",
      br(),br(),
      "1. The tree view shown to the right is based on the selected model on the sidebar menu. ",
      br(),br(),
      "2. Clicking on the chevron buttons next to a folder will expand/collapse the content view ",
      "for that particular folder.",
      br(),br(),
      "3. Clicking on DataObjects > Objects displays a quick view of the available slots for a ",
      "given RDF without having to scroll through the slot list on the sidebar menu."
    ),
    box(
      shinyTree("rdfTree")  
    )
  )
)
############################################################################################
# DEFINE DASHBOARD BODY REPORTS TAB
############################################################################################
reportsTab <- tabItem(
  tabName = "reports",
  h2("Reports"),
  "Notes:",
  br(),
  "1. This is currently a place holder tab for preconfigured reports that will be programmed ",
  " in the future.",
  br(),
  "2. These preconfigured reports will allow users to automatically generate and save graphs, ",
  "datatables, and basic analysis given a slot or loop through all the slots in an RDF. ",
  br(),
  "3. This tab will also have options to generate a preconfigured set of graphs and analysis ",
  "typicaly produced for the USBR UC & LC operations and planning models. The idea is to ",
  "make the report generator smart enough to know from the RDF metadata whether to allow ",
  "the generation of 24MS, MTOM, or CRSS reports.",
  br(),br(),
  fluidRow(
    box(
      "BASIC REPORT GENERATION OPTIONS GOES HERE..."
    ),
    box(
      "PRECONFIGURED REPORT GENERATION OPTIONS GOES HERE... THIS SHOULD BE AUTOMATICALLY HIDDEN ",
      "IF THE LOADED RDF IS NOT GENERATED BY 24MS, MTOM, OR CRSS BASED ON RDF METADATA..."
    )
  )
)
############################################################################################
# POPULATE DASHBOARD
############################################################################################
userInterface <- dashboardPage(
  # DASHBOARD HEADER
  dbHeader,
  # DASHBOARD SIDEBAR
  dbSidebar,
  # DASHBOARD BODY
  dashboardBody(
    tabItems(
      homeTab,
      graphTab,
      tableTab,
      treeTab,
      reportsTab
    )
  )
)