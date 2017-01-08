#############################################################################################################################################################
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############------------------------------------------------- Setup Module 2: Categories ------------------------------------------------------#############
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############################################################################################################################################################
#'
#' SUMMARY: 
#' 
#' User adjusts weights for categories
#'
#' MODULE MAP:
#'   
#' UI
#' 
#' (1)  ---- user presented with a pie chart for each of their focus areas, subdivided into their selected categories, with '+/-' buttons that can be used
#'      ---- to adjust the weights of the categories.
#' (2)  ---- user presses buttons until each category is appropriately weighted
#' (3)  ---- user presses 'Next'
#'
#' 
#' SERVER
#' 
#' (0)  ---- main module calls pie chart module as many times as there are focus areas
#' (1)  ---- each pie chart module creates one observer for its pie chart and a dynamic number of observers for each of its '+/-' buttons.
#' (2)  ---- i3 weights re-adjusted
#' (3)  ---- transition to next module
#' 
#' KEY COMPONENTS:
#' 
#' -- USER INTERFACE
#' ---- PIEBOX MODULE
#' ---- MAIN MODULE
#' 
#' NOTES:
#' 
#' 

##################################################################################################################################################################
#############------------------------------------------------------------- PIEBOX MODULE ------------------------------------------------------------#############
##################################################################################################################################################################

#' Define function that outputs a chosen number of colors picked from a gradient

colfunc <- colorRampPalette(c("slategray1", "deepskyblue4"))

#' ====================================================================| USER INTERFACE |====================================================================
#'  
#' Generate one simple box with a pie chart and '+' and '-' buttons

pieboxUI <- function(id, ID) {
  
  ns <- NS(id) #' namespace
  
  #' ---------------------------------------| STORAGE |-------------------------------------------
  #' 
  #' Define function to be called, and select and store the IDs of the relevant categories to 
  #' making the box 
  
  #' Define function to create button pairs for each category in module focus area
  
  prioritySet <- function(x) {
    
    column(6, column(3, h4(x)), 
           column(6, offset = 1, 
                  column(1, actionButton(ns(paste0("goplus_", x)), label = h4("+  "), width = "35px")),
                  column(1, offset = 1, actionButton(ns(paste0("gominus_", x)), label = h4("-  "), width = "35px"))
           )
    )
    
  }
  
  #' Find IDs of categories within focus area selected for module and store
  
  IDs <- unlist(A$i3$ID[A$i3$i2 == ID])
  IDs <- IDs[unlist(A$i3$Name[A$i3$i2 == ID]) != "Uncategorized"] 
  
  #' -----------------------------------| GENERATE BOX UI |----------------------------------------
  #'
  #' Call box ui with one name-spaced pie chart output and as many sets of buttons as necessary.
  
  if (length(IDs) > 1) {
    
    box(width = 4, title = ID, plotOutput(ns("pie")), fluidRow(tagList(lapply(IDs, prioritySet))))
    
  }
  
}

#' =====================================================================| SERVER |=====================================================================
#'  
#' Create the buttons' observers, and one observer to control the pie chart in the box

piebox <- function(input, output, session, selectedNode) {
  
  #' ---------------------------------------------| SETUP |---------------------------------------------------
  
  #' Find IDs of categories, within focus area selected for module, and store
  
  IDs <- unlist(A$i3$ID[A$i3$i2 == selectedNode])
  IDs <- IDs[unlist(A$i3$Name[A$i3$i2 == selectedNode]) != "Uncategorized"] 
  
  #' Proxy reactive object
  
  rv <- reactiveValues()
  
  rv$i3_weights <- A$i3$Weight[A$i3$ID %in% IDs] %>% unlist
  
  
  #' ---------------------------------------| PLUS/MINUS OBSERVERS |-------------------------------------------
  #'
  #' Create two lists of observers, one for the '+' and one for the '-' buttons, and express them
  
  #' Use IDs to generate '+' observers corresponding to every category 
  
  lapply(IDs, function (x) {
    
    #' Observer adds some to target category weight but controls sum of category weights to remain 
    #' equal to 1.
    
    observeEvent(input[[paste0("goplus_", x)]], {
      
      #' Add .01 to weight of category with ID == x
      
      A$i3$Weight[A$i3$ID == x][[1]] <- A$i3$Weight[A$i3$ID == x][[1]] + .01
      
      #' Subtract what was added to previous category to overall, but from all areas equally
      
      for (i in IDs[!(IDs %in% x)]) {
        
        A$i3$Weight[A$i3$ID == i][[1]] <- A$i3$Weight[A$i3$ID == i][[1]] - (.01)/(length(IDs)-1)
        
      }
      
      #' Reset proxy reactive object
      
      rv$i3_weights <- A$i3$Weight[A$i3$ID %in% IDs] %>% unlist
      
    })
    
  })
  
  #' Use IDs to generate '-' observers corresponding to every category 
  
  lapply(IDs, function (x) {
    
    #' Observer subtracts some from target category weight but controls sum of category weights to remain 
    #' equal to 1.
    
    observeEvent(input[[paste0("gominus_", x)]], {
      
      #' Subtract .01 to weight of category with ID == x

      A$i3$Weight[A$i3$ID == x][[1]] <- A$i3$Weight[A$i3$ID == x][[1]] - .01
      
      #' Add what was subtracted from previous category to overall, but from all areas equally
      
      for (i in IDs[!(IDs %in% x)]) {
        
        A$i3$Weight[A$i3$ID == i][[1]] <- A$i3$Weight[A$i3$ID == i][[1]] + (.01)/(length(IDs)-1)
        
      }
      
      #' Reset proxy reactive object
      
      rv$i3_weights <- A$i3$Weight[A$i3$ID %in% IDs] %>% unlist
      
    })
    
  })
  
  #' -----------------------------------------| DNYAMIC PIE CHART |--------------------------------------------
  #'
  #' Controlling for 1 or fewer user-submitted categories (in which case no pie chart), generate dynamic pie
  #' chart that responds to pressing '+' or '-' buttons.
    
    if (length(IDs) > 1) {
      
      output$pie <- renderPlot({
        
        #' Save data frame used to generate pie chart, depending on reactive value rv$weights.
        
        dfT <- data.frame(subjects = IDs, value = rv$i3_weights)
        
        ggplot(dfT, aes(x = "", y = value, fill = as.character(IDs))) + geom_bar(width = 2, stat = "identity") +
          coord_polar("y", start=0) + scale_fill_manual(values = colfunc(length(IDs))) + 
          theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank())
        
      })
      
    }
  
}

##################################################################################################################################################################
#############-------------------------------------------------------------- MAIN MODULE --------------------------------------------------------------############
##################################################################################################################################################################

#' ====================================================================| USER INTERFACE |====================================================================
#'  
#' Display some instructions and call dynamic number of pieboxUIs

setup_categoriesUI <- function(id, label = "categories") {
  
  ns <- NS(id)
  
  focusGroups <- unlist(A$i2$ID[A$i2$Name != "Uncategorized"])
  
  rows <- tagList()
  rows[[1]] <- fluidRow(
    column(5, offset = 1, h2("Step 2: Prioritizing your categories."), 
           h4("Use the buttons below to set the dials on your focus area categories to an appropriate priority level."), 
           h4("If your focus area is a class, set its categories to things like 'Homework', 'Test', etc, and set their 
              priorities to the weighting offered by your class syllabus. Click 'Next' only when you are done setting 
              all the dials (one of the dials may not respond to your input; this is a bug, but it will not affect any of
              your other categories)")), br()
           )
  rows[[2]] <- fluidRow(column(width = 10, offset = 1, lapply(focusGroups, function(id) {pieboxUI(ns(id), id)})))
  rows
  
}

#' ========================================================================| SERVER |========================================================================
#'  
#' Call dynamic number of piebox (server) modules

setup_categories <- function(input, output, session) {
  
  focusGroups <- unlist(A$i2$ID[A$i2$Name != "Uncategorized"])
  
  lapply(focusGroups, function(id) {
    
    #' Call piebox server, give it an identification, and 
    
    callModule(piebox, id, selectedNode = id)
    
  })
  
}

