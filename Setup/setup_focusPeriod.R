################################################################################################################################
#############------------------------------------------------------------------------------------------------------#############
#############------------------------------- Setup Module 3: Focus Period and Tasks -------------------------------#############
#############------------------------------------------------------------------------------------------------------#############
################################################################################################################################

#' This is the setup module for setting the length of the first focus period and choosing the tasks that will compose it.

setup_focusPeriodUI <- function(id) {
  
  ns <- NS(id)
  
  rows <- tagList()
  rows[[1]] <- fluidRow(
    column(10, offset = 1, 
           h2({"Step 3: Setting up focus period and creating tasks."}),
           h4({"You will now select a date range for your first focus period. At the moment, it is best to make this
             at least three weeks in length."})
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
  
}

setup_focusPeriod <- function(input, output, session, proceed) {
  
  ns <- session$ns
  
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
      
      tags$div(id = ns("add"),  rows )
                                   })
                                   })
  
  inserted <- reactiveValues(tasknum = 1)
  
  observeEvent(input$addtask, priority = 3, {
    
    insertUI(
      selector = paste0("#", ns("add")),
      where = "afterEnd",
      
      ui = {
        
        cols <- tagList()
        cols[[1]] <- column(2, offset = 1, 
                            textInput(ns(paste0("task", inserted$tasknum)), label = "Task name"))
        cols[[2]] <- column(2, selectInput(ns(paste0("focusArea", inserted$tasknum)), 
                                           choices = unlist(A$fdat$fa), label = "Focus Area"))
        cols[[3]] <- column(2, uiOutput(ns(paste0("categoryUI", inserted$tasknum))))
        cols[[4]] <- column(2, numericInput(ns(paste0("hours", inserted$tasknum)), 
                                            value = 0, label = "Hours"))
        cols[[5]] <- column(2, numericInput(ns(paste0("minutes", inserted$tasknum)), 
                                            value = 0, label = "Minutes"))
        
        fluidRow(cols)
        
      }
      
    )
    
    observe(priority = 2, {
      
      counter <- isolate(inserted$tasknum)
      
      index <- paste0("categoryUI", counter)
      focusArea <- paste0("focusArea", counter)
      
      output[[ index ]] <- renderUI({
        
        selectInput(ns(paste0("category", counter)), label = "Category",
                    choices = unlist(A$fdat$cat[A$fdat$fa == input[[focusArea]] ]))
        
      })
      
    })
    
  })
  
  observeEvent(input$addtask, priority = 1, {inserted$tasknum <- inserted$tasknum + 1})
  
  observeEvent(proceed(), priority = 3, {
    
    num <- inserted$tasknum - 1
    
    req(input[[paste0("task", num)]])
    
    inputsT <- lapply(1:num, function(x) {input[[paste0("task", x)]]})
    inputsF <- lapply(1:num, function(x) {input[[paste0("focusArea", x)]]})
    inputsC <- lapply(1:num, function(x) {input[[paste0("category", x)]]})
    inputsE <- lapply(1:num, function(x) {
      
      hours <- as.difftime(input[[paste0("hours", x)]], units = "hours") 
      minutes <- as.difftime(input[[paste0("minutes", x)]], units = "mins")
      Est <- hours + minutes
      units(Est) <- "mins"
      Est
      
    })
    inputsStart <- as.list(rep(as.POSIXct(Sys.time()), num))
    inputsEnd <- as.list(rep(as.POSIXct(Sys.time()), num))
    
    A$tdat <- data.frame(cbind(tasks = inputsT, ID = as.list(1000:(1000 + num - 1)), fa = inputsF, cat = inputsC, est = inputsE, 
                               start = inputsStart, end = inputsEnd))
    
    
    for (i in A$tdat$fa[!(duplicated(A$tdat$fa))]) {
      
      A$fdat$tasks[A$fdat$fa == i][[1]] <- unlist(A$tdat$tasks[A$tdat$fa == i])
      
      for (x in A$fdat$cat[A$fdat$fa == i][[1]]) {
        
        if (unlist(A$tdat$tasks[(A$tdat$cat == x) & (A$tdat$fa == i)]) %>% length == 0) {
          
        } else {
          
          A$cdat$tasks[(A$cdat$cat == x) & (A$cdat$fa == i)][[1]] <- unlist(A$tdat$tasks[(A$tdat$cat == x) & (A$tdat$fa == i)])
        }
      }
      
    }
    
    
  })
  
}



