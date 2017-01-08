#################################################################################################################################################################
#############---------------------------------------------------------------------------------------------------------------------------------------#############
#############-------------------------------------------------- Setup Module 4: Scheduling tasks ---------------------------------------------------#############
#############---------------------------------------------------------------------------------------------------------------------------------------#############
#################################################################################################################################################################
#'
#' SUMMARY:
#' 
#' User adjusts sliders that set start and end points for their tasks, observing the effects of this on a dynamic plot of their work distribution.
#' 
#' MODULE MAP:
#' 
#' UI 
#' 
#' (1) ---- User presented with double-ended sliders matched to their tasks; each end controls the start and end points of a task, respectively; as well as a
#'     ---- plot of their work distribution (in hours per day)
#' (2) ---- User adjusts a slider
#' (3) ---- Work Distribution plot shifts to reflect adjustment
#' (4) ---- User adjusts a slider again, return to step (2)
#' (5) ---- User presses 'Next'
#' 
#' SERVER
#' 
#' (1) ---- Observers created for every slider and for the work distribution chart.
#' (2) -------- Appropriate slider observer invalidates, activates, and stores values of both ends of the slider as the Start and End points of the slider's 
#'     -------- task, respectively (in the atoms data frame).
#'     -------- Plot observer invalidates, activates, retreives Start and End points from atoms data frame, and updates chart
#' (5) ---- Transition to next module
#' 
#' KEY COMPONENTS:
#' 
#' SLIDER OBSERVERS
#' PLOT OBSERVERS
#' 
#' NOTES:
#' 

setup_taskScheduleUI <- function(id) {
  
  ns <- NS(id) # namespace function
  
  #' List of unique names with format "[focus area]_[category]_[task]", for use with sliders.
  #' 
  #' --------------
  
  IDs <- A$iH[[length(A$iH)]]$ID %>% unlist %>% as.character
  
  IDs <- lapply(IDs, function(n) {
    
    atoms <- A$iH[[length(A$iH)]]
    
    i <- n
    j <- atoms$i3[atoms$ID == n] %>% unlist
    k <- atoms$i2[atoms$ID == n] %>% unlist
    
    paste(k, j, i, sep = "-")
    
  }) %>% unlist
  
  #' --------------
  #'
  
  #' Function to create slider-box, with unique identifier as title
  
  makeSlider <- function(x) {
    
    box(width = 3, sliderInput(inputId = ns(paste0("slider_", x)), label = x, value = as.POSIXct(A$fps[[1]]),
                               min = as.POSIXct(A$fps[[1]][1]), 
                               max = as.POSIXct(A$fps[[1]][2]))
    )
    
  }
  
  #' ui page structure to host sliders
  #' 
  #' -----------------
  
  rows <- tagList()
  rows[[1]] <- fluidRow(
    column(10, offset = 1, 
           h2({"Step 4: Planning the focus period"}),
           h4({"The last thing that remains to be done before Ascendance is set up on your computer is deciding 
             how to split up the estimated hours you assigned to your tasks. Ascendance makes this very simple;
             just adjust the sliders beneath each of the tasks until you arrive at a work distribution that works 
             for you. When you would like to start, just press 'Go'. (Note that you will need to follow the protocol 
             laid out at the beginning of this module's script file in order to have this module record any of your 
             data. This includes stopping the app now.)"})
    )
  )
  rows[[2]] <- br()
  rows[[3]] <- fluidRow(column(10, offset = 1, lapply(IDs, function(x) {makeSlider(x)})))
  rows[[4]] <- fluidRow(column(10, offset = 1, plotOutput(ns("workDist"))))
  rows
  
  #' ------------------
  
}

#' Server function needs to create many observers, one for each slider, as well as a plot observer that reacts to the sliders

setup_taskSchedule <- function(input, output, session, proceed, Path) {
  
  ns <- session$ns
  
  rv <- reactiveValues(dat = NULL)
  
  #' ================================================================| SLIDER OBSERVERS |=================================================================
  #'  
  #' Create reactive list of observers each of which responds to one slider.
  
  {
    
    IDs <- A$iH[[length(A$iH)]]$ID %>% unlist %>% as.character
    
    lapply(IDs, function(n) {
      
      atoms <- A$iH[[length(A$iH)]]
      
      i <- n
      j <- atoms$i3[atoms$ID == n] %>% unlist
      k <- atoms$i2[atoms$ID == n] %>% unlist
      
      observeEvent(input[[paste0("slider_", paste(k, j, i, sep = "-"))]], priority = 2, {
        
        A$iH[[length(A$iH)]]$Start[atoms$ID == n][[1]] <- (input[[paste0("slider_", paste(k, j, i, sep = "-"))]])[1] %>% as.POSIXct(., origin = "1970-01-01 00:00.00 UTC")
        A$iH[[length(A$iH)]]$End[atoms$ID == n][[1]] <- (input[[paste0("slider_", paste(k, j, i, sep = "-"))]])[2] %>% as.POSIXct(., origin = "1970-01-01 00:00.00 UTC")
        
        #' -------------------------------------------------| opCurve |-----------------------------------------------------
        #' 
        #' Create optimal curve data frame and store in Ascendance general environment (AGE).
        {
          
          opMod <- BuildOpCurve(A$iH[[length(A$iH)]], dim_extent = "time", dim_metricS = "time", units_extent = "hours", metricJump = "days")
          
          dat <- opMod[,2:ncol(opMod)] %>% rowSums %>% data_frame(fp = opMod$fp, full = .)
          
          rv$dat <- dat
          
        }
        #' --------------------------------------------------| Save |--------------------------------------------------------
        #'
        #' Save AGE to master filepath.
        
        save(A, file = Path)
        
      })
      
    })
    
  }
  
  
  #' ==================================================================| PLOT OBSERVER |===================================================================
  #'
  #' Set up single observer to react to any one of the sliders and update its graph accordingly.
  
  {
    
    output$workDist <- renderPlot({
      
      ggplot(data = rv$dat, aes(x = fp, y = full)) + geom_bar(stat = "identity") + ggtitle("Workload per day") + 
        theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
        scale_x_datetime(breaks = rv$dat$fp, labels = format(rv$dat$fp,  "%a %d"), name = NULL) + 
        scale_y_continuous(name = NULL)
      
    })
  }
  
  
}

