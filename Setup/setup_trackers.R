#############################################################################################################################################################
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############--------------------------------------------- Setup Module 1: Define hierarchies --------------------------------------------------#############
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############################################################################################################################################################
#'
#' SUMMARY: 
#' 
#' User selects from a list of checkboxes which kinds of objectives they want to keep track of.
#'
#' MODULE MAP:
#' 
#' UI
#' 
#' (1) ---- user presented with widgets to select things they want to keep track of  
#'     ---- checkboxInputs
#'     -------- Time
#'     -------- Grades
#'     -------- Money
#'     ---- textInput for "other"
#'     ---- actionButton for "add another"
#' (2) ---- user fills in information
#' (3) ---- user presses 'Next'
#'
#' 
#' SERVER
#' 
#' 
#' KEY COMPONENTS:
#' 
#' NOTES:
#' 
#' 


##################################################################################################################################################################
#############------------------------------------------------------------ User interface ------------------------------------------------------------#############
##################################################################################################################################################################

setup_trackersInput <- function(id, label = "focusAreas") {
  
  ns <- NS(id)
  
  rows <- tagList()
  rows[[1]] <- br()
  rows[[2]] <- fluidRow(
    column(9, offset = 1, 
           h3({"Please indicate below what personal resources you would like to keep track of:"})
    )
  )
  
  rows[[3]] <- br()
  
  rows[[4]] <- fluidRow(
    
    column(8, offset = 1,  checkboxGroupInput(ns("standard"), label = "", 
                                              choices = c("Time", "Grades", "Money"), 
                                              selected = c("Time", "Grades"),
                                              width = '80%')
           )
    )
  rows[[5]] <- br()
  rows[[6]] <- fluidRow(
    
    column(8, offset = 1, h4("Type any additional categories into this box, separating them with commas (e.g. 'meal points, calorie intake'):"), 
           textInput(ns("other"), label = "", width = "98%"))
    
  )
  rows[[7]] <- br()
  rows[[8]] <- fluidRow(
    
    column(8, offset = 1, h4("When you are done, click 'Next'."))
    
  )
  rows[9:15] <- lapply(9:15, function(x) {br()})
  
  Rows <- tagList()
  Rows[[1]] <- fluidRow(
    
    column(10, offset = 1, h1({"Welcome to Ascendance"}))
    
  )
  Rows[[2]] <- br()
  Rows[[3]] <- fluidRow(
    
    column(8, offset = 2, h3({"Ascendance is a personal resource manager, helping you manage your goals with intuitive graphics that don't force you to choose between
      accountability and flexibility."}),
           h3({"In the next several windows, you will be telling Ascendance how it should organize your personal information. 
             The first step is to decide what kinds of information to keep track of."})
           )
    
  )
  Rows[[4]] <- br()
  Rows[[5]] <- column(8, offset = 2, rows)
  
  Rows
  
}

##################################################################################################################################################################
#############---------------------------------------------------------------- Server -----------------------------------------------------------------############
##################################################################################################################################################################

setup_trackers <- function(input, output, session, proceed) {
  
  ns <- session$ns
  
  observeEvent(proceed(), priority = 2, {
    
    ns("test") %>% print
    
    req(input$other)
    
    items <- VectorizeString(input$other) %>% append(input$standard, . )
    
    #########################################################
    
    if (is.null(A$rv[["0"]])) { 
      A$rv <- list(list(Names = c(), Location = c())) %>% append(A$rv, . )
      names(A$rv)[length(A$rv)] <- "0"
    }
    
    NamesIndex <- items %in% A$rv[["0"]][["Names"]]
    LocationIndex <- rep(ns(""), length(items)) %in% A$rv[["0"]][["Location"]]
    OverallIndex <- !NamesIndex & !LocationIndex
    
    A$rv[["0"]][["Names"]] <- append(A$rv[["0"]][["Names"]], items[OverallIndex])
    A$rv[["0"]][["Location"]] <- append(A$rv[["0"]][["Location"]], rep(ns(""), length(items))[OverallIndex])
    
    #######################################################
    
  })
  
}