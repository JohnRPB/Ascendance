################################################################################################################################
#############------------------------------------------------------------------------------------------------------#############
#############---------------------------------- Tutorial Module 1: Focus Areas ------------------------------------#############
#############------------------------------------------------------------------------------------------------------#############
################################################################################################################################

#' This is the tutorial module for creating focus areas and prioritizing them
#' 

colfunc <- colorRampPalette(c("slategray1", "deepskyblue4"))

setup_focusAreasInput <- function(id, label = "focusAreas") {
  
  ns <- NS(id)
  
  #' tutorial elements as list, including (1) intro text, (2) "submit" button and focus areas input,
  #' (3) the pop-up categories input.
  
  tutorial <- tagList()
  tutorial[[1]] <- fluidRow(
    column(10, offset = 1, 
           h2({"Step 1: Identifying and prioritizing your focus areas and dividing them into categories"}),
           h4({"For each of the following sets of inputs, 
             please fill out all the fields and hit 'submit' when you're done."}), br(), 
           h4({"For the first box, you will be choosing your focus areas. These are wide-ranging 
             categories that could be anything from classes you are taking to long-term projects
             you would like to complete."}),
           h4({"It is good to have anywhere from 3 to 6 focus areas, although a number outside this
             range is also possible."})
    )
  )
  tutorial[[2]] <- fluidRow(
    column(2, offset = 1, tags$h3("Choose focus areas:"), br(),
           actionButton(ns("submitfocus"), h4("Submit"), width = "150px")
    ),
    column(5,  
           textInput(ns("selectfocus"), label = "",  width = "600px"),
           h4("Focus areas should have no spaces and be separated by commas."),
           h4("e.g. ' Calculus, ManageBusiness, Networking' ")
    )
  )
  tutorial[[3]] <- uiOutput(ns("ui"))
  tutorial
  
}


setup_focusAreas <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  vals <- reactiveValues(x = NULL, y = NULL)
  
  makeObsvs <- eventReactive(input$submitfocus, {
    
    focusGroups <- rv$A$fdat$fa[rv$A$fdat$fa != "Uncategorized"]
    
    IDs <- unlist(focusGroups)
    
    if (is.null(vals$x)) {
      
      res1 <- lapply(IDs, function (x) {
        
        observeEvent(input[[paste0("goplus_", x)]], {
          
          rv$A$fdat$weight[rv$A$fdat$fa == x][[1]] <- rv$A$fdat$weight[rv$A$fdat$fa == x][[1]] + .01
          
          for (i in IDs[!(IDs %in% x)]) {
            
            rv$A$fdat$weight[rv$A$fdat$fa == i][[1]] <- rv$A$fdat$weight[rv$A$fdat$fa == i][[1]] - (.01)/(length(IDs)-1)
            
          }
          
          
          
        })
        
      })
      
      res2 <- lapply(IDs, function (x) {
        
        observeEvent(input[[paste0("gominus_", x)]], {
          
          rv$A$fdat$weight[rv$A$fdat$fa == x][[1]] <- rv$A$fdat$weight[rv$A$fdat$fa == x][[1]] - .01
          
          for (i in IDs[!(IDs %in% x)]) {
            
            rv$A$fdat$weight[rv$A$fdat$fa == i][[1]] <- rv$A$fdat$weight[rv$A$fdat$fa == i][[1]] + (.01)/(length(IDs)-1)
            
          }
          
          
          
        })
        
      })
      
      res <- append(res1, res2)
      
      vals$x <- 1
      vals$y <- IDs
      
    } else if (all(IDs %in% vals$y)) {
      
      return(NULL)
      
    } else {
      
      new_ind <- !(IDs %in% vals$y)
      
      res1 <- lapply(IDs[new_ind], function (x) {
        
        
        observeEvent(input[[paste0("goplus_", x)]], {
          
          rv$A$fdat$weight[rv$A$fdat$fa == x][[1]] <- rv$A$fdat$weight[rv$A$fdat$fa == x][[1]] + .01 ## add .01 to focus group weight
          
          for (i in IDs[!(IDs %in% x)]) {
            
            rv$A$fdat$weight[rv$A$fdat$fa == i][[1]] <- rv$A$fdat$weight[rv$A$fdat$fa == i][[1]] - (.01)/(length(IDs)-1)
            
          }
          
          
        })
      })
      res2 <- lapply(IDs, function (x) {
        
        observeEvent(input[[paste0("gominus_", x)]], {
          
          rv$A$fdat$weight[rv$A$fdat$fa == x][[1]] <- rv$A$fdat$weight[rv$A$fdat$fa == x][[1]] - .01
          
          for (i in IDs[!(IDs %in% x)]) {
            
            rv$A$fdat$weight[rv$A$fdat$fa == i][[1]] <- rv$A$fdat$weight[rv$A$fdat$fa == i][[1]] + (.01)/(length(IDs)-1)
            
          }
          
          
        })
        
      })
      
      res <- append(res1, res2)
      
      vals$y <- IDs
      
    }
    
    res
    
  })
  
  observeEvent(eventExpr = input$submitfocus, handlerExpr = {
    
    ns <- session$ns
    
    prioritySet <- function(id) {
      
      column(6, column(3, h4(id)), 
             column(6, offset = 1, 
                    column(1, actionButton(ns(paste0("goplus_", id)), label = h4("+  "), width = "35px")),
                    column(1, offset = 1, actionButton(ns(paste0("gominus_", id)), label = h4("-  "), width = "35px"))
             )
      )
    }
    
    dRow_managePanel <- function(id, labels) {
      fluidRow(
        column(1, offset = 1, checkboxInput(ns(paste0("class", id)), label = "", value = FALSE, width = 30)),
        column(2, h3(paste0("    ", labels))),
        column(3, textInput(ns(paste0("focusAreas", id)), label = "", width = "350px")),
        column(4, dateRangeInput(ns(paste0("focusdateAreas", id)), label = "", width = "400px")),
        column(1, checkboxInput(ns(paste0("noend", id)), label = "", value = FALSE))
      )
    }
    
    focusAreas <- unlist(strsplit(unlist(strsplit(input$selectfocus, ",")), " "))
    focusAreas <- append(focusAreas[-which(focusAreas == "")], "Uncategorized")
    
    focusAreas %>% length %>% rep("", .) %>% as.list %>% {
      
      A$fdat <- data.frame(fa = I( . ), cat = I( . ), tasks = I( . ), weight = I( . ), dateStart = I( . ), 
                           dateEnd = I( . ), class = I( . ))
      
    }
    
    A$fdat$fa <- as.list(focusAreas)
    
    A$fdat$weight[A$fdat$fa != "Uncategorized"] <- as.list(rep(1/(length(focusAreas) - 1), length(focusAreas) - 1))
    
    A$fdat[A$fdat$fa == "Uncategorized",]$weight <- NA
    
    rv$A <- A
    
    IDs <- A$fdat$fa[A$fdat$fa != "Uncategorized"]
    
    output$ui <- renderUI({
      
      rows <- tagList()
      rows[2:(length(IDs)+1)] <- lapply(1:length(IDs), function(k) {
        dRow_managePanel(id = unlist(IDs)[k], labels = paste("  ", labels = unlist(IDs))[k])
      })
      rows[[1]] <- fluidRow(column(1, offset = 1, h3("Class?")), column(3, offset = 2, h3("Categories")), 
                            column(3, h3("Dates active")), column(2, h3("  No end")))                
      rows[[length(IDs)+2]] <- fluidRow(column(11, offset = 1, h4("Enter typical contributive categories for your focus 
                                                                  areas, such as 'Homework', 'Test', and 'StudyTime' 
                                                                  (if your focus areas is a class), or 'marketing'. Select 
                                                                  'No end' if you do not have an end date in mind."), 
                                               h4("Then hit 'submit' when you have carefully reviewed 
                                                  all this information; your settings will be stored in the app 
                                                  (although nothing will happen to incidate you clicked it). 
                                                  Press the 'Next' button to move on to the next part
                                                  of the setup.")))
      rows[[length(IDs)+3]] <- fluidRow(br(), column(12, offset = 1, actionButton(ns("submitcat"), h4("Submit"), width = "150px")))
      columns <- tagList(column(1), "", column(6, rows))                                         
      columns[[2]] <- column(4, h3("Priority Settings"), box(width = 12, plotOutput(ns("pie"), height = 300), 
                                                             fluidRow(lapply(unlist(IDs), prioritySet))))
      
      columns
      
    })
    
    makeObsvs()
    
  })
  
  observeEvent({
    
    IDs <- unlist(rv$A$fdat$fa)[unlist(rv$A$fdat$fa) != "Uncategorized"]
    
    a <- lapply( IDs , function (x) {
      
      input[[paste0("goplus_", x)]]      
      
    })
    
    b <- lapply( IDs , function (x) {
      
      input[[paste0("gominus_", x)]]      
      
    }) 
    
    append(a, b)
    
  }, {
    
    output$pie <- renderPlot({
      
      focusGroup <- rv$A$fdat[rv$A$fdat$fa != "Uncategorized",]
      
      groups <- unlist(focusGroup$fa)
      
      rv$weights <- unlist(focusGroup$weight)
      
      dfT <- data.frame(subjects = groups, value = rv$weights)
      
      ggplot(dfT, aes(x = "", y = value, fill = groups)) + 
        geom_bar(width = 2, stat = "identity") + coord_polar("y", start=0) + 
        scale_fill_manual(values = colfunc(length(focusGroup$fa))) + 
        theme(axis.title.x = element_blank()) + 
        theme(axis.title.y = element_blank())
      
    })
    
  })
  
  observeEvent(input$submitcat, {
    
    focusGroups <- A$fdat$fa[A$fdat$fa != "Uncategorized"]
    
    categoriesdat <- data.frame(cat = c(), fa = c(), tasks = c(), weight = c())
    
    for (x in unlist(focusGroups)) {
      
      categories <- unlist(strsplit(unlist(strsplit(input[[paste0("focusAreas", x)]], ",")), " "))
      
      if (length(categories) == 0) {
        
        categories <- "Uncategorized"
        
      } else if (length(categories) == 1) {
        
        categories <- append(categories, "Uncategorized")
        
      } else {
        
        categories <- append(categories[-which(categories == "")], "Uncategorized")        
        
      }
      
      A$fdat$cat[A$fdat$fa == x][[1]] <- categories
      
      dat <- categories %>% length %>% rep("", .) %>% as.list %>% {
        
        data.frame(cat = I( as.list(categories) ), 
                   fa = I( as.list(rep(x, length(categories))) ), 
                   tasks = I( . ), 
                   weight = I( as.list(rep(1/(length(categories)-1), length(categories)))))
        
      }
      
      categoriesdat <- rbind(categoriesdat, dat)
      
      A$cdat <- categoriesdat
      
      A$fdat$dateStart[A$fdat$fa == x][[1]] <- (input[[paste0("focusdateAreas", x)]])[1]
      
      if (input[[paste0("noend", x)]] == TRUE) {
        
        A$fdat$dateEnd[A$fdat$fa == x][[1]] <- NA
        
      } else {
        
        A$fdat$dateEnd[A$fdat$fa == x][[1]] <- (input[[paste0("focusdateAreas", x)]])[2]
        
      }
      
      A$fdat$class[A$fdat$fa == x][[1]] <- input[[paste0("class", x)]]
      
    }
    
    A$cdat$ID <- as.list(1:nrow(A$cdat))
    
    for (x in unlist(focusGroups)) {
      
      A$cdat$weight[(A$cdat$cat == "Uncategorized") & (A$cdat$fa == x)][[1]] <- NA
      
    }
    
  })
  
}