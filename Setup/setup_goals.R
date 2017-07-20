#############################################################################################################################################################
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############--------------------------------------------- Setup Module 3: Set dynamic goals ---------------------------------------------------#############
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############################################################################################################################################################
#'
#' SUMMARY: 
#' 
#' User is presented with hierarchy pertaining to tracked quantity, where each node is a number adjustable both by '+/-' buttons and manual typing. The
#' hierarchy implements an algorithm that determines, for every node in the hierarchy, how to shift its previously assigned number, when any change is 
#' made to any node. Nodes can be "locked" with a button; this influences the algorithm. The goal is to help the user create goals that are consistent
#' at every level of the hierarchy, e.g. a total GPA consistent with the desired grades for each class.
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


########################################################################################################################################################

#' =======================================================================| STORAGE |=======================================================================
#'  
#' Store reactive environment, dynamic variable 'y', and a reactive expression using 'y' to determine which observers to create

#' ---------------------------------------| DYNAMIC MODULES LIST |-------------------------------------------
#'
#' Use i0 rows, created after user finishes first module, to generate future modules, and insert them in the 'switch' chain

setup_goalsInput <- function(id) {
  
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
    
    column(10, offset = 1, br()) # where dynamic ui code was
    
  )
  Rows[10:16] <- lapply(10:16, function(x) { br() })
  
  Overall <- tagList()
  Overall[[1]] <- br()
  Overall[[2]] <- fluidRow(column(10, offset = 1, Rows))
  Overall
  
  
}

setup_goals <- function(input, output, session, proceed, ID) {
  
  ns <- session$ns
  
}