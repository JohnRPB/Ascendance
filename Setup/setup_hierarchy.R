#############################################################################################################################################################
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############-------------------------------------------- Setup Module 2: Construct hierarchy --------------------------------------------------#############
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############################################################################################################################################################
#'
#' SUMMARY: 
#'
#' MODULE MAP:
#' 
#' UI
#' 
#' SERVER
#' 
#' KEY COMPONENTS:
#' 
#' NOTES:
#' 

recursiveModuleInput <- function(id, ID, level) {
  
  ns <- NS(id)

  nextlevel <- level + 1
  
  paste0("ID: ", ID) %>% print
  paste0("level: ", level) %>% print
  
  Title <- (A$rv[[as.character(level-1)]][["Names"]])[as.numeric(ID)]
  
  box(width = 11, title = h3(Title), status = "primary",
      column(10, offset = 1, textInput(ns( paste0("i", level, "_items") ), label = "Type more:")),
      column(10, offset = 1, actionButton(ns("submit"), label = "Submit")), br(),
      column(10, uiOutput(ns("weight_ui"))),
      column(10, offset = 1, uiOutput(ns("ui")))
  )
  
}

recursiveModule <- function(input, output, session, ID, level) {
  
  nextlevel <- level + 1
  
  ns <- session$ns
  
  ns("testing_server") %>% print
  
  prioritySet <- function(x) {
    
    column(6, column(1, h4(x)), 
           column(6, offset = 1, 
                  column(1, actionButton(ns(paste0("goplus_", x)), label = h4("+  "), width = "35px")),
                  column(1, offset = 1, actionButton(ns(paste0("gominus_", x)), label = h4("-  "), width = "35px"))
           )
    )
    
  }
  
  observeEvent(input$submit, {
    
    req(input[[paste0("i", level, "_items")]])
    
    items <- VectorizeString(input[[paste0("i", level, "_items")]])
    
    NamesIndex <- items %in% A$rv[[as.character(level)]][["Names"]]
    LocationIndex <- rep(ns(""), length(items)) %in% A$rv[[as.character(level)]][["Location"]]
    OverallIndex <- !NamesIndex | !LocationIndex
    
    IDs <- items[OverallIndex] %>% length  %>% seq_len
    
    output$weight_ui <- renderUI({
      
      fluidRow(column(10, fluidRow(lapply(IDs, function(x) {column(2, textInput(ns(paste0(x, "_weight")), label = "", width = '50px'))}))))
      
    })
    
  })
  
  
  observeEvent(input$submit, priority = 2, {
    
    req(input[[paste0("i", level, "_items")]])
    
    items <- VectorizeString(input[[paste0("i", level, "_items")]])
    
    locationVector <- strsplit(ns(""), "-")[[1]] %>% as.numeric
    
    ########################################################################################
    
    if (is.null(A$rv[[as.character(level)]])) { 
      A$rv <- list(list(Names = c(), Location = c())) %>% append(A$rv, . )
      names(A$rv)[length(A$rv)] <- as.character(level)
    }
    
    "Focus your attention here: " %>% print()
    items %>% print
    A$rv[[as.character(level)]][["Names"]] %>% print
    
    rep(ns(""), length(items)) %>% print
    A$rv[[as.character(level)]][["Location"]] %>% print    
    
    NamesIndex <- items %in% A$rv[[as.character(level)]][["Names"]]
    LocationIndex <- rep(ns(""), length(items)) %in% A$rv[[as.character(level)]][["Location"]]
    OverallIndex <- !NamesIndex | !LocationIndex
    
    A$rv[[as.character(level)]][["Names"]] <- append(A$rv[[as.character(level)]][["Names"]], items[OverallIndex])
    A$rv[[as.character(level)]][["Location"]] <- append(A$rv[[as.character(level)]][["Location"]], rep(ns(""), length(items))[OverallIndex])
    
    #########################################################################################
    
    #' quick hack
    
    numberOfNewItems <- items[OverallIndex] %>% length
    totalNumberOfItems <- A$rv[[as.character(level)]][["Names"]] %>% length
    difference <- totalNumberOfItems - numberOfNewItems
    moduleIDsToCall <- (difference+1):(difference + numberOfNewItems)
    
    output$ui <- renderUI({
      
      Rows <- tagList()
      Rows[moduleIDsToCall] <- lapply(as.character(moduleIDsToCall), 
                                      
                                      function(id) {  
                                        
                                        recursiveModuleInput(ns(id), ID = id, level = level + 1) %>% fluidRow  
                                        
                                      }) 
      Rows
      
    })
    
    for (id in moduleIDsToCall) {
      
      callModule(recursiveModule, id = id, ID = id, level = level + 1)
      
    }
    
  })
}

setup_hierarchyInput <- function(id) {
  
  ns <- NS(id)
  
  indexVec <- A$rv[["0"]]$Names %>% length %>% seq_len %>% as.character
  
  Title <- A$rv[["0"]]$Names[indexVec == id]
  
  Rows <- tagList()
  
  Rows[[1]] <- fluidRow(column(8, offset = 2, h2(Title) ))
  Rows[2:5] <- lapply(2:5, function(x) { br() })
  Rows[[6]] <- fluidRow(
    
    column(6, offset = 2, h4("Type your items into this box"), 
           textInput(ns("i1_items"), label = "", width = "98%"))
    
  )
  Rows[[7]] <- fluidRow(
    
    column(6, offset = 2, actionButton(ns("submit"), label = "Submit"))
    
  )
  Rows[[8]] <- br()
  Rows[[9]] <- fluidRow(
    
    column(10, offset = 1, uiOutput(ns("ui")))
    
  )
  Rows[10:16] <- lapply(10:16, function(x) { br() })
  
  Overall <- tagList()
  Overall[[1]] <- br()
  Overall[[2]] <- fluidRow(column(10, offset = 1, Rows))
  Overall
  
  
}

setup_hierarchy <- function(input, output, session, proceed, ID) {
  
  ns <- session$ns
  
  observeEvent(input$submit, {
    
    req(input$i1_items)
    
    items <- VectorizeString(input$i1_items)
   
    #########################################################
    
    if (is.null(A$rv[["1"]])) { 
      A$rv <- list(list(Names = c(), Location = c())) %>% append(A$rv, . )
      names(A$rv)[length(A$rv)] <- "1"
    }
    
    NamesIndex <- items %in% A$rv[["1"]][["Names"]]
    LocationIndex <- rep(ns(""), length(items)) %in% A$rv[["1"]][["Location"]]
    OverallIndex <- !NamesIndex | !LocationIndex
    
    A$rv[["1"]][["Names"]] <- append(A$rv[["1"]][["Names"]], items[OverallIndex])
    A$rv[["1"]][["Location"]] <- append(A$rv[["1"]][["Location"]], rep(ns(""), length(items))[OverallIndex])
    
    #######################################################
    
    #' quick hack
    
    numberOfNewItems <- items[OverallIndex] %>% length
    totalNumberOfItems <- A$rv[["1"]][["Names"]] %>% length
    difference <- totalNumberOfItems - numberOfNewItems
    moduleIDsToCall <- (difference+1):(difference + numberOfNewItems) %>% as.character

    output$ui <- renderUI({
      
      Rows <- tagList()
      Rows[moduleIDsToCall] <- lapply(as.character(moduleIDsToCall), 
                                      
                                      function(id) {  
                                        
                                        recursiveModuleInput(ns(id), ID = id, level = 2) %>% fluidRow  
                                        
                                      }) 
      Rows
      
    })
        
    for (id in as.character(moduleIDsToCall)) {
      
      callModule(recursiveModule, id, ID = id, level = 2)
      
    }
    
  })

  observeEvent(proceed(), priority = 3, {
    
    A$rv %>% print
    
  })
  
  
}