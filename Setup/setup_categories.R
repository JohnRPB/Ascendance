################################################################################################################################
#############------------------------------------------------------------------------------------------------------#############
#############---------------------------------- Ascendance Tutorial Main Module -----------------------------------#############
#############------------------------------------------------------------------------------------------------------#############
################################################################################################################################

#' This is the tutorial module for prioritizing categories within focus areas.
#' 

#' Pie chart box module
#' 
#' module function

#' the id of the piebox ui module is the focus group being generated for

pieboxUI <- function(id, ID) {
  
  ns <- NS(id)
  
  prioritySet <- function(x) {
    
    column(6, column(3, h4(x)), 
           column(6, offset = 1, 
                  column(1, actionButton(ns(paste0("goplus_", x)), label = h4("+  "), width = "35px")),
                  column(1, offset = 1, actionButton(ns(paste0("gominus_", x)), label = h4("-  "), width = "35px"))
           )
    )
  }
  
  IDs <- unlist(A$cdat$cat[A$cdat$fa == ID])[unlist(A$cdat$cat[A$cdat$fa == ID]) != "Uncategorized"]
  
  if (length(IDs) > 1) {
    
    box(width = 4, title = ID, plotOutput(ns("pie")), fluidRow(tagList(lapply(IDs, prioritySet))))
    
  }
  
}

# the piebox server must be passed the focus area of the box in the argument 'ID'

piebox <- function(input, output, session, ID) {
  
  rv <- reactiveValues()
  
  rv$A <- A
  
  IDs <- unlist(A$cdat$cat[A$cdat$fa == ID])[unlist(A$cdat$cat[A$cdat$fa == ID]) != "Uncategorized"]
  
  lapply(IDs, function (x) {
    
    observeEvent(input[[paste0("goplus_", x)]], {
      
      rv$A$cdat$weight[rv$A$cdat$cat == x][[1]] <- rv$A$cdat$weight[rv$A$cdat$cat == x][[1]] + .01
      
      for (i in IDs[!(IDs %in% x)]) {
        
        rv$A$cdat$weight[rv$A$cdat$cat == i][[1]] <- rv$A$cdat$weight[rv$A$cdat$cat == i][[1]] - (.01)/(length(IDs)-1)
        
      }
      
    })
    
  })
  
  lapply(IDs, function (x) {
    
    observeEvent(input[[paste0("gominus_", x)]], {
      
      rv$A$cdat$weight[rv$A$cdat$cat == x][[1]] <- rv$A$cdat$weight[rv$A$cdat$cat == x][[1]] - .01
      
      for (i in IDs[!(IDs %in% x)]) {
        
        rv$A$cdat$weight[rv$A$cdat$cat == i][[1]] <- rv$A$cdat$weight[rv$A$cdat$cat == i][[1]] + (.01)/(length(IDs)-1)
        
      }
      
    })
    
  })
  
  observeEvent({
    
    IDs <- unlist(rv$A$cdat$cat[rv$A$cdat$fa == ID])[unlist(rv$A$cdat$cat[rv$A$cdat$fa == ID]) != "Uncategorized"]
    
    if (length(IDs) > 1) {
      
      a <- lapply(IDs, function (x) {
        
        input[[paste0("goplus_", x)]]      
        
      })
      
      b <- lapply(IDs, function (x) {
        
        input[[paste0("gominus_", x)]]      
        
      })
      
      append(a, b)
      
    }
    
  }, {
    
    if (length(IDs) > 1) {
      
      output$pie <- renderPlot({
        
        rv$cat_weights <- unlist(rv$A$cdat$weight[(rv$A$cdat$fa == ID) & (rv$A$cdat$cat != "Uncategorized")])
        
        dfT <- data.frame(subjects = IDs, value = rv$cat_weights)
        
        ggplot(dfT, aes(x = "", y = value, fill = IDs)) + geom_bar(width = 2, stat = "identity") +
          coord_polar("y", start=0) + scale_fill_manual(values = colfunc(length(IDs))) + 
          theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank())
        
      })
      
    }
    
  })
  
}

#' 

setup_categoriesUI <- function(id, label = "categories") {
  
  ns <- NS(id)
  
  focusGroups <- unlist(A$fdat$fa[A$fdat$fa != "Uncategorized"])
  
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

setup_categories <- function(input, output, session) {
  
  focusGroups <- unlist(A$fdat$fa[A$fdat$fa != "Uncategorized"])
  
  lapply(focusGroups, function(id) {
    
    callModule(piebox, id, ID = id)
    
  })
  
}