# processAnalyticsShinyDashboard
Shiny dashboard consisting of process analytics toolset based on bupaR

bupaR: https://www.bupar.net/
processanimatoR: https://github.com/fmannhardt/processanimateR/
shiny: https://shiny.rstudio.com/

___

# known issues:
processanimaterOutput function cannot be combined with the processMap. The procesMap throws an error:  
"TypeError: Cannot call a class as a function"  
  
This bug is already repoted at the processanimatoR developer: https://github.com/fmannhardt/processanimateR/issues/16  
  
comment-out the processanimatorOutput when using the processMap.

___

### Main panel with filtering methods
![alt text](https://github.com/kooskaspers/processAnalyticsShinyDashboard/blob/master/md/main.png "Main panel")
### Event log
![alt text](https://github.com/kooskaspers/processAnalyticsShinyDashboard/blob/master/md/event-log.png "Event log")
### Process map
![alt text](https://github.com/kooskaspers/processAnalyticsShinyDashboard/blob/master/md/process-map.png "Process map")
### Performance map
![alt text](https://github.com/kooskaspers/processAnalyticsShinyDashboard/blob/master/md/performance-map.png "Performance map")
### Process animation
![alt text](https://github.com/kooskaspers/processAnalyticsShinyDashboard/blob/master/md/process-animation.png "Process animation")