#################################################################################################################################################################
#############---------------------------------------------------------------------------------------------------------------------------------------#############
#############--------------------------------------- Setup Module 3: Setting focus period and adding tasks -----------------------------------------#############
#############---------------------------------------------------------------------------------------------------------------------------------------#############
#################################################################################################################################################################
#'
#' SUMMARY:
#' 
#' MODULE MAP:
#' 
#' UI 
#' 
#' (1)  ---- user presented with box to enter start and end dates of focus period
#' (2)  ---- user selects dates
#' (3)  ---- user hits 'Submit'
#' (4)  ---- text and an 'add task' button appears; user is prompted to click the button
#' (5)  ---- user clicks the 'add task' button
#' (6)  ---- new row appears, containing text input for user to name task, drop-down inputs for focus area and category, and numeric inputs for hours and min 
#'      ---- estimates
#' (7)  ---- user enters name and selects focus area from drop-down menu
#' (8)  ---- user selects category from drop-down menu that has its option set by previous focus area choice, then enters time estimates into min and hour input
#' (9)  ---- user presses 'add task' again, cycle returns to (5)
#' (10) ---- user presses 'Next'
#' 
#' 
#' SERVER
#' 
#' (0)  ---- observer created to generate ui of step (4) when user clicks 'Submit'; observer created to respond to click of 'Add task' button in step (5)
#' (3)  ---- reactive ui generated
#' (5)  ---- new row inserted beneath 'Add task' button; observer created for 'Category' drop-down menu
#' (7)  ---- 'Category' drop-down fills in with the categories in the focus area selected
#' (9)  ---- return to (5)
#' (10) ---- information from all task rows retrieved and processes to make i4 and atoms; move to next-module
#' 
#' KEY COMPONENTS:
#' 
#' INSTRUCTIONS FOR ADDING TASKS
#' ADD NEW TASK ROW
#' UPDATE COUNTER
#' RETRIEVE TASKS
#' 
#' NOTES:
#' 
#' 

##################################################################################################################################################################
#############------------------------------------------------------------ User interface ------------------------------------------------------------#############
##################################################################################################################################################################

setup_focusPeriodUI <- function(id) {
  
  ns <- NS(id) # namespace function
  
  #' create 
  #' 
  #' ---------------------
  
  rows <- tagList() 
  
  rows[[1]] <- fluidRow(
    
    column(10, offset = 1, 
           h2("Step 3: Setting up focus period and creating tasks."),
           h4("You will now select a date range for your first focus period. At the moment, it is best to make this
             at least three weeks in length.")
    )
    
  )
  
  rows[[2]] <- fluidRow(
    
    column(1, offset = 1, 
           actionButton(ns("submit"), h4("Submit"))
    ),
    
    column(5,  
           dateRangeInput(ns("focusrange"), label = "",  width = "600px")
    )
    
  )
  
  rows[[3]] <- uiOutput(ns("taskintro"))
  
  rows
  
  #' ---------------------
  #' 
  
}

##################################################################################################################################################################
#############---------------------------------------------------------------- Server -----------------------------------------------------------------############
##################################################################################################################################################################


setup_focusPeriod <- function(input, output, session, proceed) {

  #' ========================================================================| SETUP |=======================================================================
  #'  
  #' Namespace and add counter to track numer of rows created
  
  #' Namespace fuction must be retrieved from session to be used in reactive UI
  
  ns <- session$ns
  
  #' Counter keeps track of number of rows created, to be used in distinguishing inputs from each row.
  
  inserted <- reactiveValues(tasknum = 0)
  
  #' ================================================================| GENERATE INSTRUCTIONS |================================================================
  #'  
  #' Generate row of text instructing user how to assign tasks, and produce an 'add task' button.
  
  observeEvent(input$submit, {
    
    A$fps <- list(input$focusrange)
    
    output$taskintro <- renderUI({
      
      rows <- tagList()
      
      rows[[1]] <- fluidRow(column(10, offset = 1, 
                                   h4({"Congratulations, you have initiated your first focus period! The next part will
                                     require some planning and forethought; think about all of the tasks and 
                                     activities you would like to complete during this time. Press the 'Add Task'
                                     button to generate new task input panels, and fill out all the fields. You
                                     will be asked to estimate the amount of time it will take to complete each
                                     task; try to make these estimates as precise as possible."}), br(),
                                   h4({"Continue adding tasks until you have listed all of the ones you can think of
                                     for your focus period. Then hit 'next' to advance to the final stage of 
                                     the setup."})))
      
      rows[[2]] <- fluidRow(
        
        column(2, offset = 1, 
               
               actionButton(ns("addtask"), "Add Task")
               
        )
        
      )
      
      rows[[3]] <- br()
      
      #' place a div around this reactive ui and call it; the div will be used as a separation point that InsertUI will place new rows after, later.
      
      tags$div(id = ns("add"),  rows )
      
    })
    
  })
  
  #' ===================================================================| UPDATE COUNTER |====================================================================
  #'  
  #' Add one to counter for every click of 'Add task' button.
  
  observeEvent(input$addtask, priority = 4, {inserted$tasknum <- inserted$tasknum + 1})
  
  #' ==================================================================| ADD NEW TASK ROW |===================================================================
  #'  
  #' Generate new row after click of 'add task' button; row has text-input for name, linked drop-down menus for focus area and category, and numeric-inputs
  #' for hour and minute estimates.  
  
  observeEvent(input$addtask, priority = 3, {
    
    #' ---------------------------------------------| SETUP |----------------------------------------------------
    
    #' Store counter for use as a secondary, row-specific name-space, and isolate it so it doesn't invalidate the observer of the reactive ui it will be used in
    
    counter <- isolate(inserted$tasknum)
    
    
    #' -----------------------------------------| INSERT NEW ROW |-----------------------------------------------
    #'
    #'  Insert a new row of inputs for next task
    
    insertUI(
      
      #' Put the new row directly after the div, "focusperiod-add" (the 'Add task' and instructions div)
      
      selector = paste0("#", ns("add")),
      
      #' At the end of it
      
      where = "afterEnd",
      
      ui = {
        
        cols <- tagList()
        cols[[1]] <- column(2, offset = 1, 
                            textInput(ns(paste0("task", counter)), label = "Task name"))
        cols[[2]] <- column(2, selectInput(ns(paste0("focusArea", counter)), 
                                           choices = unlist(A$i2$Name), label = "Focus Area"))
        cols[[3]] <- column(2, uiOutput(ns(paste0("categoryUI", counter))))
        cols[[4]] <- column(2, numericInput(ns(paste0("hours", counter)), 
                                            value = 0, label = "Hours"))
        cols[[5]] <- column(2, numericInput(ns(paste0("minutes", counter)), 
                                            value = 0, label = "Minutes"))
        
        fluidRow(cols)
        
      }
      
    )

    #' -------------------------| SET UP OBSERVER FOR 'CATEGORY' DROP-DOWN MENU |--------------------------------
    #' 
    #' Populate 'Category' drop-down menu with categories from focus area selected in 'Focus Area' drop-down menu
    
    #' Use row-specific name for focus area input
    
    focusArea <- paste0("focusArea", counter)
    
    #' Ensure name of observer is row-specific
    
    index <- paste0("categoryUI", counter)
    
    output[[ index ]] <- renderUI({
      
      #' Get ID of focus area for which name equals user selection
      
      focusID <- A$i2$ID[ A$i2$Name == input[[focusArea]] ][[1]]
      
      #' Use focus area ID to subset category names in i3
      
      choices <- A$i3$Name[A$i3$i2 == focusID] %>% unlist
      
      selectInput(ns(paste0("category", counter)), label = "Category",
                  choices = choices)
      
    })
    
    
  })
  
  #' ===================================================================| RETRIEVE TASKS |====================================================================
  #'  
  #' Grab all input from all task rows; build i4 and atoms data frames; and bind all data frames into one. Also build RawLog.
  
  observeEvent(proceed(), priority = 3, {
    
    #' Save counter
    
    counter <- inserted$tasknum
    
    #' check if the counter row exists
    
    req(input[[paste0("task", counter)]])
   
    #' -------------------------------------------------| n-hierarchy |-------------------------------------------------------
    #'
    #' Make 'RawLog' object and store it in AGE.
     
    #' Generate lists necessary to build i4 and atom data frames, cycling through available reactive inputs row by row
    #'
    #' ------------

    #' Task names
    
    inputsT <- lapply(1:counter, function(x) {input[[paste0("task", x)]]})
    
    #' Focus area IDs
    
    inputsF <- lapply(1:counter, function(x) {A$i2$ID[A$i2$Name == input[[paste0("focusArea", x)]] ][[1]]})
    
    #' Category IDs
    
    inputsC <- lapply(1:counter, function(x) {
      
      #' If focus area is NOT "Uncategorized", produce task's category ID as normal; otherwise, produce NA
      
      if (isTruthy(input[[paste0("category", x)]])) {
      
      A$i3$ID[A$i3$Name == input[[paste0("category", x)]] ][[1]]
      
      } else {
        
        NA
        
      }
        
      })
    
    #' Time estimates
    
    inputsE <- lapply(1:counter, function(x) {
      
      hours <- as.difftime(input[[paste0("hours", x)]], units = "hours") 
      minutes <- as.difftime(input[[paste0("minutes", x)]], units = "mins")
      Est <- hours + minutes
      units(Est) <- "mins"
      Est
      
    })
    
    #' Dummy start and end points
    
    inputsStart <- as.list(rep(as.POSIXct(A$fps[[1]][1]), counter))
    inputsEnd <- as.list(rep(as.POSIXct(A$fps[[1]][2]), counter))
    
    #' ------------
    #'
    
    #' Build i4
    
    A$i4 <- as_tibble(cbind(ID = as.list(1:(counter)), Name = inputsT, Notes = rep("", counter), Start = inputsStart, End = inputsEnd, Weight = rep(1/counter, counter),
                            i0 = rep(1, counter), i1 = rep(1, counter), i2 = inputsF, i3 = inputsC))
    
    #' Build atoms
    
    A$atoms <- as_tibble(cbind(ID = as.list(1:(counter)), Name = rep("", counter), Notes = rep("", counter), Extent = inputsE, Start = inputsStart, End = inputsEnd,
                               Weight = rep(0, counter), Class = rep("P", counter), i0 = rep(1, counter), i1 = rep(1, counter), i2 = inputsF, i3 = inputsC, 
                               i4 = as.list(1:(counter))))
    
    #' Bind all into one n-hierarchy list object
    
    A$iH <- list(A$i0, A$i1, A$i2, A$i3, A$i4, A$atoms)
    
    
    #' -------------------------------------------------| RawLog |-------------------------------------------------------
    #'
    #' Make 'RawLog' object and store it in AGE.
    {
      
      atoms <- A$iH[[length(A$iH)]]
      
      RawLog <- matrix(ncol = length(atoms$ID), nrow = 0) %>% as_tibble 
      colnames(RawLog) <- atoms$ID %>% as.character
      
      suppressWarnings(RawLog[] <- lapply(1:ncol(RawLog), function(x) { RawLog[,x] <- as.list(RawLog[,x]) }))
      
      RawLog <- lapply(1:4, function(x) { RawLog })
      
      names(RawLog) <- c("Auto", "Start", "End", "LogTime")
      
      A$RawLog <- RawLog
      
    }
    
    #' Clean up AGE
    
    rm("atoms", "i0", "i1", "i2", "i3", "i4", envir = A)
    
  })
  
}



