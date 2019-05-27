library(shiny)
library(shinydashboard)
library(bupaR)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)
library(processanimateR)
source("translateEventlogFHS.R")
source("translateEventlogISD.R")

sessionInfo()

# fhs dossiers
event_log_df <-
  read.csv("C:/padata/OfferteAcceptatie/12mnd.csv",
           stringsAsFactors = FALSE,
           sep = ";")

# DOCS
# event_log_df <-
#   read.csv("C:/padata/Docs/data.csv",
#            stringsAsFactors = FALSE,
#            sep = ";")

# ISD ("~" as column seperator, because ";" is already used in the incident descriptions)
# event_log_df <-
#   read.csv("C:/padata/ISD/data.csv",
#            stringsAsFactors = FALSE, header = TRUE, quote="", fill=FALSE,
#            sep = "~")


# event_log_df$ACTIVITY[event_log_df$ACTIVITY == "H01. Hibabejelentés megkezdve"] <- "test"
# 
# event_log_df$ACTIVITY[event_log_df$ACTIVITY == "H01. Hibabejelentés megkezdve"] <- "test2"


#translate NL -> EN in dataframe FHS
# event_log_df <- translateEventlogFHS(event_log_df)

#translate HU -> EN in dataframe ISD
# event_log_df <- translateEventlogISD(event_log_df)

#### FILTER DATAFRAME TO ONLY 1000 RECORDS ######
#### can be useful when working with large datasets
 # print("row filter applied!!")
 # event_log_df <- tail(event_log_df, 20000)

# modify dataframe to make it compatible with bupaR
event_log_df <- mutate(event_log_df, status = "complete") # add status = complete, there is no transactional lifecycle
event_log_df <- mutate(event_log_df, activity_instance_id = 1:nrow(event_log_df)) # add unique id to activity_instance_id
event_log_df <- mutate(event_log_df, TIMESTAMP = ymd_hms(TIMESTAMP, truncated = 3)) # fix the timestamp notation, so it's compatible with bupaR

# cast amount to numeric, if column 'amount' exists. otherwise continue
try({event_log_df$AMOUNT = as.numeric(as.character(event_log_df$AMOUNT))}, silent = TRUE)

#create eventLog
event_log <- bupaR::eventlog(eventlog = event_log_df,
                             case_id = "CASE",
                             activity_id = "ACTIVITY",
                             activity_instance_id = "activity_instance_id",
                             timestamp = "TIMESTAMP",
                             lifecycle_id = "status",
                             resource_id = "RESOURCE")

#describe data types
# sapply(event_log, class)
# sapply(event_log, class)

server <- function(input, output) { 
  
  
  ####RENDER DATE RANGE INPUT####
  
  output$outputTimePeriodRange <- renderUI({
    dateRangeInput("inputTimePeriodRange", "2. b. Date range:", start = min(event_log$TIMESTAMP), end = max(event_log$TIMESTAMP) + days(1), min = min(event_log$TIMESTAMP) - days(1), max = max(event_log$TIMESTAMP) + days(2))
  })
  

  ####RENDER ACTIVITY PRESENCE FILTER####
  
  output$outputActivityPresenceFilterRange <- renderUI({
    
    # choicesList2 <- unique(event_log$ACTIVITY) #how to add 'Off' here?
    # levels(choicesList) <- c("None", levels(choicesList))
    
    selectizeInput(
      "inputFilterActivityPresence",
      label = NULL, #no title, we already have a title defined in the ui.R which has a different font
      selected = NULL,
      multiple = TRUE,
      options = list(placeholder = 'select 1 or more'),
      choices = c(levels(unique(event_log$ACTIVITY))) 
    )
  })
  
  ####RENDER ACTIVITY PRESENCE ANTECEDENT FILTER####
  
  output$outputActivityPrecedenceAntecedent <- renderUI({
  
    selectizeInput(
      "inputFilterActivityPrecedenceAntecedent",
      "Antecedent",
      multiple = TRUE,
      selected = NULL,
      choices = c(levels(unique(event_log$ACTIVITY))),
      options = list(
        placeholder = "",
        maxItems = 1)
    )
  })
  
  ####RENDER ACTIVITY PRESENCE CONSEQUENT FILTER####
  
  output$outputActivityPrecedenceConsequent <- renderUI({
    selectizeInput(
      "inputFilterActivityPrecedenceConsequent",
      "Consequent",
      multiple = TRUE,
      selected = NULL,
      choices = c(sort(levels(unique(event_log$ACTIVITY)))),
      options = list(
        placeholder = "",
        maxItems = 1)
    )
  })
  
  ####RENDER SINGLE FILTER DROPDOWN ####
  
  output$outputFilterType <- renderUI({
    selectInput(
      "inputFilterType",
      "Filter on",
      selected = "None",
      choices = c("None", colnames(event_log))
    )
  })
  
  ####RENDER SINGLE FILTER VALUE BOX (ADAPTIVE, BASED ON FILTER TYPE IT WILL RENDER ) ####
  ####This is not really generic, because I list some custom fields here. Woulbe be more generic
  ####If the selectizeInput function for the filterValue is generated dynamically on all filterTypes.
  
  output$outputFilterValue1 <- renderUI({
    print(input$inputFilterType)
    if (is.null(input$inputFilterType)){
      NULL
    }
    else if (input$inputFilterType == "None"){
      NULL
    }
    else if (input$inputFilterType == "CASE"){
      textInput(
        "inputFilterValue",
        "Value"
      )
    }
    else if (input$inputFilterType == "AMOUNT"){
      sliderInput(
        "inputFilterValue",
        "Amount",
        min = min(event_log$AMOUNT, na.rm = TRUE),
        max = max(event_log$AMOUNT, na.rm = TRUE),
        value = c(min,max)
      )
    }
    else if (input$inputFilterType == "TYPE"){
      selectizeInput(
        "inputFilterValue",
        "Value",
        selected = "None",
        multiple = TRUE,
        choices = sort(unique(event_log$TYPE))
      )
    }
    else if (input$inputFilterType == "TP"){
      selectizeInput(
        "inputFilterValue",
        "Value",
        selected = "None",
        multiple = TRUE,
        choices = sort(unique(event_log$TP))
      )
    }
    else if (input$inputFilterType == "COMPANY"){
      selectizeInput(
        "inputFilterValue",
        "Value",
        selected = "None",
        multiple = TRUE,
        choices = sort(unique(event_log$COMPANY))
      )
    }
    else if (input$inputFilterType == "BUSINESS_UNIT"){
      selectizeInput(
        "inputFilterValue",
        "Value",
        selected = "None",
        multiple = TRUE,
        choices = sort(unique(event_log$BUSINESS_UNIT))
      )
    }
    else if (input$inputFilterType == "ORGANIZATION"){
      selectizeInput(
        "inputFilterValue",
        "Value",
        selected = "None",
        multiple = TRUE,
        choices = sort(unique(event_log$ORGANIZATION))
      )
    }
    else if (input$inputFilterType == "ISSUE_TITLE"){
      selectizeInput(
        "inputFilterValue",
        "Value",
        selected = "None",
        multiple = TRUE,
        choices = sort(unique(event_log$ISSUE_TITLE))
      )
    }
    else {
      textInput(
        "inputFilterValue",
        "Value"
      )
      
    }
    
  })
  

  ####APPLY FILTERS TO EVENT_LOG####
  
  #no, not reactive anymore. because we want to apply filters upfront generating the event_log.
  # filtered_event_log <- reactive({
  #   
  # })
  
  applyFilters <- function(input_event_log) {
    print("applying filters")
    #### SINGLE FILTER ####
    print(paste("inputFilterType: ",input$inputFilterType))
    #don't take inputFilterType into account when filter is set to "None"
    if (input$inputFilterType != "None"){
      print(paste("Applying single filter. Checking number of remaining cases: ",nrow(count(input_event_log, CASE))))
      if (input$inputFilterType == "AMOUNT"){
        input_event_log <- filter(input_event_log, AMOUNT %in% (input$inputFilterValue[1]:input$inputFilterValue[2]))
      }
      else{
        input_event_log <- filter(input_event_log, !!rlang::sym(input$inputFilterType) == input$inputFilterValue) #found the solution here to have a dyamic variable: https://stackoverflow.com/questions/49786597/r-dplyr-filter-with-a-dynamic-variable-name
      }
      print(paste("Cases left after applying single filter: ",nrow(count(input_event_log, CASE))))
    }
    
    
    #### ACTIVITY PRESENCE FILTER ####
    #don't take inputFilterType into account when filter is set to "None"
    if (length(input$inputFilterActivityPresence) != 0){ #if the selectedInput is set to multiple=true, an empty inputbox gives length = 0 (true NULL)
      print(paste("Applying precense filter. Checking number of remaining cases: ",nrow(count(input_event_log, CASE))))
      if (nrow(count(input_event_log, CASE)) != 0 ){
        input_event_log <- filter_activity_presence(input_event_log, input$inputFilterActivityPresence)
        print(paste("Cases left after applying presence filter: ",nrow(count(input_event_log, CASE))))
      }
      else{
        print("There are no cases left to apply the presence filter upon, skipping presence filter.")
      }
    }
    
    if (length(input$inputFilterPrecedenceType) != 0){ #if the selectedInput is set to multiple=false, an empty inputbox gives length = 1 (value = '')
      print(paste("Applying precedence filter. Checking number of remaining cases: ",nrow(count(input_event_log, CASE))))
      if (nrow(count(input_event_log, CASE)) != 0 ){
        print(paste("Antecedent: ",input$inputFilterActivityPrecedenceAntecedent))
        print(paste("Precedence: ",input$inputFilterActivityPrecedenceConsequent))
        print(paste("Precedence type: ",input$inputFilterPrecedenceType))
        input_event_log <- filter_precedence(input_event_log, antecedents = input$inputFilterActivityPrecedenceAntecedent, consequents = input$inputFilterActivityPrecedenceConsequent, precedence_type = input$inputFilterPrecedenceType)
        print(paste("Cases left after applying precedence filter: ",nrow(count(input_event_log, CASE))))
      }
      else{
        print("There are no cases left to apply the precedence filter upon, skipping precedence filter.")
      }

    }
    
    #### TIME FILTER ####
    if (input$inputFilterMethod != ''){
      print(paste("Applying time filter. Checking number of remaining cases: ",nrow(count(input_event_log, CASE))))
      if (nrow(count(input_event_log, CASE)) != 0 ) {
        input_event_log <- filter_time_period(input_event_log, interval = c(input$inputTimePeriodRange[1], input$inputTimePeriodRange[2]), filter_method = input$inputFilterMethod)
        input_event_log <- filter_trace_frequency(input_event_log, percentage = (input$inputTraceFrequencyPerc / 100), reverse = F)
        print(paste("Cases left after applying time filter: ",nrow(count(input_event_log, CASE))))
        
      }
      else{
        print("There are no cases left to apply the time filter upon, skipping time filter.")
      }
    }
    
    #### THROUGHPUT FILTER ####
    if (input$inputThroughputTimeRangeUnits != ''){
      print(paste("Applying throughput filter. Checking number of remaining cases: ",nrow(count(input_event_log, CASE))))
      if (nrow(count(input_event_log, CASE)) != 0 ) {
        input_event_log <- filter_throughput_time(input_event_log, interval = c(input$inputThroughputTimeStart, input$inputThroughputTimeEnd), units = input$inputThroughputTimeRangeUnits)
        # input_event_log <- filter_throughput_time(input_event_log, interval = c(50, 100), units = "days")
        print(paste("Cases left after applying throughput filter: ",nrow(count(input_event_log, CASE))))
      }
      else{
        print("There are no cases left to apply the time filter upon, skipping time filter.")
      }
    }
    
    input_event_log <- input_event_log %>%

      #### EVENT FILTERS ####
      filter_resource_frequency(percentage = (input$inputResourceFrequence / 100)) %>%
      filter_activity_frequency(percentage = (input$inputActivityFrequence / 100))
    return(input_event_log)
  }
  
  
  ####TRIGGER APPLY FILTERS FUNCTION WHEN RECEIVED CALL FROM BUTTON####
  
  observeEvent(input$start, {
    print(nrow(count(event_log, CASE)))
    print("Received trigger for applying filters")
    print(paste("Cases in event_log:", nrow(count(event_log, CASE))))
    # update the filtered_event_log with the output of applying filters
    filtered_event_log <- applyFilters(event_log)
    print(paste("Filters applied, remaining cases:", nrow(count(filtered_event_log, CASE))))
    renderNonReactiveOutputs(filtered_event_log)
  })
  
  # output$selectedOption <- renderText({
  #   paste(input$sidebarmenu)
  # })
  
  # observeEvent(input$animation, {
  #   print("Animation")
  #   
  #     output$processAnimation <- renderProcessanimater({
  #       if (nrow(filtered_event_log) == 0){
  #         return(print("No cases meet the criteria"))
  #       }
  #       animate_process(patients, mode = "absolute", jitter = 10, legend = "color",
  #                       mapping = token_aes(color = token_scale("TYPE",
  #                                                               scale = "ordinal",
  #                                                               range = RColorBrewer::brewer.pal(7, "Paired"))))
  #     })
  #   
  #   
  # })

  
  #### RENDER THE NON REACTIVE OUTPUTS ####
  renderNonReactiveOutputs <- function(event_log){
    
    #numberOfCases field
    output$numberOfCases <- renderText({
      return(nrow(count(event_log, CASE)))
    })
    
    #numberofEvents field
    
    output$numberOfEvents <- renderText({
      # if (is.null(input$inputTimePeriodRange)){
      #   return(print(""))
      # }
      return(nrow(event_log))
    })
    
    #processMap
    
    # output$processMap <- renderUI({
    #   
    #   if (is.null(input$inputTimePeriodRange)){
    #     return(print("One moment.."))
    #   }
    #   
    #   if (nrow(event_log) == 0){
    #     return(print("No cases meet the criteria"))
    #   }
    #   
    #   tagList(
    #     process_map(event_log, type = frequency(input$inputFreqValue), rankdir = input$inputRankDir)
    #     
    #   )
    # })
    
    output$processMap <- renderGrViz({
      
      if (is.null(input$inputTimePeriodRange)){
        return(print("One moment.."))
      }
      
      if (nrow(event_log) == 0){
        return(print("No cases meet the criteria"))
      }
      
        process_map(event_log, type = frequency(input$inputFreqValue), rankdir = input$inputRankDir)
    })
    
    ####RENDER EVENTLOG
    output$eventLog <- renderTable({
      return(event_log)
    })
  
    
    ####RENDER PERFORMANCE MAP#####
    
    output$performanceMap <- renderGrViz({
      
      if (is.null(input$inputTimePeriodRange)){
        return(print("One moment.."))
      }

      if (nrow(event_log) == 0){
        return(print("No cases meet the criteria"))
      }
        process_map(event_log, rankdir = input$inputRankDirPerformance, performance(FUN = mean, units=input$inputUnitsPerformanceMap), color_scale = RColorBrewer::brewer.pal(3, "Accent"))
    })
    
    ####RENDER RESOURSE MAP#####
    
    output$resourceMap <- renderUI({
      tagList(
        event_log %>%
        resource_map( type = frequency(input$inputFreqValueResourceMap), rankdir = input$inputRankDirResourceMap)
      )
    })
    
    ###RENDER DOTTED CHART
    
    # rommel werkt niet
    output$dottedChart <- renderGrViz({
        event_log %>%
          dotted_chart(x = "relative", y = "duration")
    })
    
    ### RENDER PROCESS ANIMATION
    
    output$processAnimation <- renderProcessanimater({
      if (is.null(input$inputTimePeriodRange)){
        return(print("One moment.."))
      }

      if (nrow(event_log) == 0){
        return(print("No cases meet the criteria"))
      }
      animate_process(event_log, mode = "absolute", jitter = 10, legend = "color",
                      mapping = token_aes(color = token_scale("TYPE",
                                                              scale = "ordinal",
                                                              range = RColorBrewer::brewer.pal(7, "Paired"))))
    })
    
  }
  

  
  #### RENDER THE REACTIVE OUTPUTS ####
  
  ### Number of cases in 'Filters' tab
  output$numberOfCasesDataset <- renderText({
    if (is.null(input$inputTimePeriodRange)){
      return(print(""))
    }
    nrow(count(event_log, CASE))
  })
  
  ### Download handler in 'Event log' tab
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_event_log", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(applyFilters(event_log), file, row.names = FALSE)
    }
  )
  

  
}
