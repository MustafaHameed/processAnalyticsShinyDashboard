library(shiny)
library(shinydashboard)
library(processanimateR)
library(DiagrammeR)

header <- dashboardHeader(title = "Process Analytics",
                          titleWidth = 300)

sidebar <- dashboardSidebar(
  # textOutput("selectedOption"),
  sidebarMenu(id = "sidebarmenu",
    menuItem("Filters", tabName = "filters", icon = icon("filter")),
    menuItem("Event log", icon = icon("fas fa-th"), tabName = "eventLog",
             badgeColor = "blue"),
    menuItem("Process map", tabName = "processMap", icon = icon("random")),
    menuItem("Performance map", icon = icon("dashboard"), tabName = "performanceMap",
             badgeColor = "green"),
    menuItem("Process Animation", icon = icon("users"), tabName = "processAnimation"),
    menuItem("Resource map", icon = icon("users"), tabName = "resourceMap",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Dotted chart", icon = icon("route"), tabName = "dottedChart",
             badgeLabel = "new", badgeColor = "green")
  ),
  width = 300
)

# body <- dashboardBody(h2(htmlOutput("processMap")))

body <- dashboardBody(
  tags$head(tags$style(HTML(".grViz { width:100%!important;height:100%!important;}"))),
  tabItems(
    tabItem(tabName = "filters", 
            fluidRow(
              valueBox(textOutput("numberOfCasesDataset"), "Cases in dataset", icon = icon("bars"), color = "yellow", width = 4),
              valueBox(textOutput("numberOfCases"), "Cases filtered", icon = icon("bars"), color = "yellow", width = 4),
              valueBox(textOutput("numberOfEvents"), "Events filtered", icon = icon("list"), color = "yellow", width =4)
            ),
            fluidRow(            
              box(
              title = "Single filters", background = "purple", width = 2, collapsible = T, collapsed = F,
              uiOutput("outputFilterType"),
              uiOutput("outputFilterValue1")
            ),
            # uiOutput("outputFilterValue2"),
            box(
              title = "Activity filters", background = "olive", width = 2, collapsible = T, collapsed = F,
              h4("Presence"),
              uiOutput("outputActivityPresenceFilterRange"),
              hr(),
              h4("Precedence"),
              uiOutput("outputActivityPrecedenceAntecedent"),
              uiOutput("outputActivityPrecedenceConsequent"),
              selectizeInput(
                inputId = "inputFilterPrecedenceType",
                label = "Precedence type",
                multiple = TRUE,
                selected = NULL,
                choices = c("directly_follows", "eventually_follows"),
                options = list(
                  placeholder = "",
                  maxItems = 1)
              )
            ),
            box(
              title = "Time filter", background = "light-blue", width = 2, collapsible = T, class = "collapsed-box",
              selectInput(
                "inputFilterMethod",
                "Method",
                selected = NULL,
                choices = c('', "contained", "start", "complete", "intersecting", "trim") 
              ),
              uiOutput("outputTimePeriodRange")
            ),
            box(
              title = "Case filters", background = "maroon", width = 2, collapsible = T, class = "collapsed-box",
              sliderInput(
                "inputTraceFrequencyPerc",
                "Trace frequency (%)",
                min = 1,
                max = 100,
                value = 80
              ),
              hr(),
              h4("Throughput time:"),
              numericInput(
                "inputThroughputTimeStart",
                "Start:",
                value = NULL
              ),
              numericInput(
                "inputThroughputTimeEnd",
                "End:",
                value = NULL
              ),
              selectInput(
                "inputThroughputTimeRangeUnits",
                "Units:",
                selected = NULL,
                choices = c('', "mins",  "hours", "days")
              )
            ),
            box(
              title = "Event filters", background = "orange", width = 2, collapsible = T, class = "collapsed-box",
              sliderInput(
                "inputResourceFrequence",
                "Resource freq (%)",
                min = 1,
                max = 100,
                value = 100
              ),
              sliderInput(
                "inputActivityFrequence",
                "Activity freq (%)",
                min = 1,
                max = 100,
                value = 100
              )
            ),
            actionButton("start", "Apply filters", icon = icon('filter'), width = '10%' )
            )

            ),
    tabItem(tabName = "processMap",
            fluidRow(
              box(
                width = 3, background = "yellow",
                radioButtons(
                  "inputRankDir",
                  "Direction of graph",
                  choiceNames = c("Left-right", "Top-down"),
                  choiceValues = c("LR", "TB"),
                  inline = F
                )
              ),
              box(
                width = 3, background = "yellow",
                selectInput(
                  "inputFreqValue",
                  "Frequency value:",
                  choices = c("absolute",  "absolute_case", "relative", "relative_case")
                )
              )
            ),
            
            fluidRow(
              box(
                width = 12, title = "Process Map", 
                status = "primary", 
                solidHeader = T,
                grVizOutput("processMap", width = "100%", height = "100%"),
                align = "center")
            )
    ),
    
    tabItem(tabName = "performanceMap",
            fluidRow(
              box(
                width = 3, background = "yellow",
                radioButtons(
                  "inputRankDirPerformance",
                  "Direction of graph",
                  choiceNames = c("LeftRight", "TopDown"),
                  choiceValues = c("LR", "TB"),
                  inline = T
                )
              ),
              box(
                width = 3, background = "yellow",
                selectInput(
                  "inputUnitsPerformanceMap",
                  "Units:",
                  selected = "days",
                  choice = c("mins", "hours", "days","weeks")
                )
              )
            ),
            
            fluidRow(
              
              
              box(
                width = 12, title = "Performance Map", 
                status = "primary", 
                solidHeader = T,
                grVizOutput("performanceMap", width = "100%", height = "100%"),
                align = "center")

            )
    ),
    tabItem(tabName = "dottedChart",
            fluidRow(
              box(
                width = 12, title = "Trace explorer", 
                status = "primary", 
                solidHeader = T,
                grVizOutput("dottedChart", width = "100%", height = "100%"),
                align = "center")
            )
    ),
    
    tabItem(tabName = "eventLog",
            downloadButton("downloadData", "Download"),
            tableOutput("eventLog")

    ),
    
    tabItem(tabName = "processAnimation",
            # processanimaterOutput("processAnimation") ## this works, but breaks the procesmap! this is a bug in processanimateR, see: https://github.com/fmannhardt/processanimateR/issues/16
            NULL
    ),
    
    tabItem(tabName = "resourceMap",
            fluidRow(
              box(
                width = 3, background = "yellow",
                radioButtons(
                  "inputRankDirResourceMap",
                  "Direction of graph",
                  choiceNames = c("LeftRight", "TopDown"),
                  choiceValues = c("LR", "TB"),
                  inline = T
                )
              )
              ,
              box(
                width = 3, background = "yellow",
                selectInput(
                  "inputFreqValueResourceMap",
                  "Frequency value:",
                  choices = c("absolute",  "absolute_case", "relative", "relative_case")
                )
              )
            ),
            fluidRow(
              box(
                width = 12, title = "Resource Map", 
                status = "primary", 
                solidHeader = T,
                h2(htmlOutput("resourceMap")),
                align = "center")
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)
