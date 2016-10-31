#############################################################################################################################################################
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############------------------------------------------------ Setup Module 1: Focus Areas ------------------------------------------------------#############
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############################################################################################################################################################

## ------------------------------------------------------------------------------------------------------------------------------------------------------- ##
## --------------------------------------------------------------------------------------------------------------------------------------------- ##
## ----------------------------------------------------------------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------------------------------------------------------- ##
## --------------------------------------------------------------------------------------------------------------- ##

#' This is the tutorial module for creating focus areas and prioritizing them
#' 

colfunc <- colorRampPalette(c("slategray1", "deepskyblue4"))

## ----------------------------------------------------------------- Main input function ----------------------------------------------------------------- ##

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

## ---------------------------------------------------------------- Main server function ----------------------------------------------------------------- ##

setup_focusAreas <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  vals <- reactiveValues(x = NULL, y = NULL)
  
  ## --------------------------------------------------- Reactive expression for '+/-' buttons --------------------------------------------------- ##
  
  makeObsvs <- eventReactive(input$submitfocus, {
    
    #' Create vector of only weighted focus area IDs (i.e. exclude 'Uncategorized')
    
    IDs <- rv$A$i2$ID[rv$A$i2$Name != "Uncategorized"] %>% unlist
    
    ## ------------------------------------------- Conditional for generating '+/-' observers -------------------------------------------- ##
    
    #' Conditional allows for control of multiple presses of 'submit' button (in case user re-types and re-submits).
    #' 
    #' This is accomplished via storage of variables x and y in 'vals' reactiveValues object (see above); when x is NULL, program knows 
    #' 'submit' button was never pressed, while y stores the IDs of the focus areas that have already been generated.
    
    ## --------------------------------------------------- First condition ----------------------------------------------------- ##
    
    ##' If observers for buttons have been generated before
    
    if (is.null(vals$x)) {
      
      ## -------------------------------------- Iterate to store multiple observers ------------------------------------ ##
      
      ##' res1 stores observer list for '+' buttons
      ##' res2 for '-' buttons
      
      res1 <- lapply(IDs, function (x) {
        
        ## -------------------------------------- Generate a '+' observer -------------------------------------- ##
        
        ##' Observer adds some to target focus area weight but controls sum of focus area weights to remain 
        ##' equal to 1.
        
        observeEvent(input[[paste0("goplus_", x)]], {
          
          #' Add .01 to weight of focus Area with ID == x
          
          rv$A$i2$Weight[rv$A$i2$ID == x][[1]] <- rv$A$i2$Weight[rv$A$i2$ID == x][[1]] + .01
          
          #' Subtract what was added to previous focus area to overall, but from all areas equally
          
          for (i in IDs[!(IDs %in% x)]) {
            
            rv$A$i2$Weight[rv$A$i2$ID == i][[1]] <- rv$A$i2$Weight[rv$A$i2$ID == i][[1]] - (.01)/(length(IDs)-1)
            
          }
          
        })
        
      })
      
      res2 <- lapply(IDs, function (x) {
        
        ## -------------------------------------- Generate a '-' observer -------------------------------------- ##
        
        ##' Observer subtracts some from target focus area weight but controls sum of focus area weights to remain equal 
        ##' to 1.
        
        observeEvent(input[[paste0("gominus_", x)]], {
          
          #' Subtract .01 to weight of focus Area with ID == x
          
          rv$A$i2$Weight[rv$A$i2$ID == x][[1]] <- rv$A$i2$Weight[rv$A$i2$ID == x][[1]] - .01
          
          for (i in IDs[!(IDs %in% x)]) {
            
            #' Add what was subtracted from previous focus area to overall, but from all areas equally
            
            rv$A$i2$Weight[rv$A$i2$ID == i][[1]] <- rv$A$i2$Weight[rv$A$i2$ID == i][[1]] + (.01)/(length(IDs)-1)
            
          }
          
        })
        
      })
      
      #' Combine and store observer lists
      
      res <- append(res1, res2)
      
      #' Notify program that observers have been created
      
      vals$x <- 1
      
      #' Notify program of *which* focus areas observers were created for
      
      vals$y <- IDs
      
      ## --------------------------------------------------- Second condition ---------------------------------------------------- ##
      
      ##' Ff 'new' list of IDs is the same as old list
      
    } else if (all(IDs %in% vals$y)) {
      
      return(NULL)
      
      ## ---------------------------------------------------- Third condition ---------------------------------------------------- ##
      
      ##' If 'new' list of IDs has some of the same numbers as old list
      
    } else {
      
      #' Create var of IDs for which observers have not already been generated
      
      new_ind <- !(IDs %in% vals$y)
      
      ## -------------------------------------- Iterate to store remaining observers ----------------------------------- ##
      
      res1 <- lapply(IDs[new_ind], function (x) {
        
        ## -------------------------------------- Generate a '+' observer -------------------------------------- ##
        
        ##' Observer adds some to target focus area weight but controls sum of focus area weights to remain 
        ##' equal to 1.
        
        observeEvent(input[[paste0("goplus_", x)]], {
          
          #' Add .01 to weight of focus Area with ID == x
          
          rv$A$i2$Weight[rv$A$i2$ID == x][[1]] <- rv$A$i2$Weight[rv$A$i2$ID == x][[1]] + .01 ## add .01 to focus group weight
          
          #' Subtract what was added to previous focus area to overall, but from all areas equally
          
          for (i in IDs[!(IDs %in% x)]) {
            
            rv$A$i2$Weight[rv$A$i2$ID == i][[1]] <- rv$A$i2$Weight[rv$A$i2$ID == i][[1]] - (.01)/(length(IDs)-1)
            
          }
          
        })
        
      })
      
      res2 <- lapply(IDs, function (x) {
        
        ## -------------------------------------- Generate a '-' observer -------------------------------------- ##
        
        ##' Observer subtracts some from target focus area weight but controls sum of focus area weights to remain 
        ##' equal to 1.
        
        observeEvent(input[[paste0("gominus_", x)]], {
          
          #' Subtract .01 to weight of focus Area with ID == x
          
          rv$A$i2$Weight[rv$A$i2$ID == x][[1]] <- rv$A$i2$Weight[rv$A$i2$ID == x][[1]] - .01
          
          #' Add what was subtracted from previous focus area to overall, but from all areas equally
          
          for (i in IDs[!(IDs %in% x)]) {
            
            rv$A$i2$Weight[rv$A$i2$ID == i][[1]] <- rv$A$i2$Weight[rv$A$i2$ID == i][[1]] + (.01)/(length(IDs)-1)
            
          }
          
        })
        
      })
      
      ## ------------------------------------- Combine observers and update objects  ----------------------------------- ##
      
      #' Combine and store observer lists
      
      res <- append(res1, res2)
      
      #' Notify program of *which* focus areas observers were created for
      
      vals$y <- IDs
      
    }
    
    ## ------------------------------------------------------ Express observers ---------------------------------------------------------- ##
    
    res
    
  })
  
  ## ------------------------------------------------------------------------------------------------------------------------------- ##
  
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
    
    #######
    #' transitioning to n-level hierachy
    
    A$i0 <- tibble(
      ID = list(1, 2), 
      Name = list("Time", "GPA"), 
      Notes = list("Keep track of temporal things", "Keep track of grades and GPA"),
      End = list(NA, NA),
      Start = list(Sys.time(), Sys.time()),
      Weight = list(.5, .5)
    )
    
    A$i1 <- tibble(
      ID = list(1, 2), 
      Name = list("Productive", "Leisure"), 
      Notes = list("Self-explanatory", "Self-explanatory"),
      End = list(NA, NA),
      Start = list(Sys.time(), Sys.time()),
      Weight = list(.5, .5),
      i0 = list(1,1)
    )
    
    ######
    
    focusAreas %>% length %>% rep("", .) %>% as.list %>% {
      
      A$i2 <- tibble(
        ID = . , Name = . , Notes = . , End = . , Start = . , Weight =. , i0 = . , i1 = .
      )
      
    }
    
    A$i2$i0 <- focusAreas %>% length %>% rep(1, . ) %>% as.list
    
    A$i2$i1 <- focusAreas %>% length %>% rep(1, . ) %>% as.list
    
    A$i2$ID <- focusAreas %>% length %>% seq_len %>% as.list
    
    A$i2$Name <- focusAreas %>% as.list
    
    A$i2$Weight[A$i2$Name != "Uncategorized"] <- as.list(rep(1/(length(focusAreas) - 1), length(focusAreas) - 1))
    
    A$i2[A$i2$Name == "Uncategorized",]$Weight <- NA
    
    rv$A <- A
    
    IDs <- A$i2$ID[A$i2$Name != "Uncategorized"]
    
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
    
    IDs <- unlist(rv$A$i2$Name)[unlist(rv$A$i2$Name) != "Uncategorized"]
    
    a <- lapply( IDs , function (x) {
      
      input[[paste0("goplus_", x)]]      
      
    })
    
    b <- lapply( IDs , function (x) {
      
      input[[paste0("gominus_", x)]]      
      
    }) 
    
    append(a, b)
    
  }, {
    
    output$pie <- renderPlot({
      
      focusGroup <- rv$A$i2[rv$A$i2$Name != "Uncategorized",]
      
      groups <- unlist(focusGroup$Name)
      
      rv$Weights <- unlist(focusGroup$Weight)
      
      dfT <- data.frame(subjects = groups, value = rv$Weights)
      
      ggplot(dfT, aes(x = "", y = value, fill = groups)) + 
        geom_bar(width = 2, stat = "identity") + coord_polar("y", start=0) + 
        scale_fill_manual(values = colfunc(length(focusGroup$Name))) + 
        theme(axis.title.x = element_blank()) + 
        theme(axis.title.y = element_blank())
      
    })
    
  })
  
  observeEvent(input$submitcat, {
    
    focusGroups <- A$i2$ID[A$i2$Name != "Uncategorized"]
    
    categoriesdat <- tibble(ID = list(), Name = list(), Notes = list(), End = list(), Start = list(), Weight = list(), 
                            i0 = list(), i1 = list(), i2 = list())
    
    for (x in unlist(focusGroups)) {
      
      categories <- unlist(strsplit(unlist(strsplit(input[[paste0("focusAreas", x)]], ",")), " "))
      
      if (length(categories) == 0) {
        
        categories <- "Uncategorized"
        
      } else if (length(categories) == 1) {
        
        categories <- append(categories, "Uncategorized")
        
      } else {
        
        categories <- append(categories[-which(categories == "")], "Uncategorized")        
        
      }
      
      dat <- categories %>% length %>% rep("", .) %>% as.list %>% {
        
        tibble(ID = as.list(seq_len(length(categories))),
               Name =  as.list(categories) , 
               Notes =  . , 
               End = . ,
               Start = . ,
               Weight = as.list(rep(1/(length(categories)-1), length(categories))),
               i0 = as.list(rep(1, length(categories))) ,
               i1 = as.list(rep(1, length(categories))) ,
               i2 = as.list(rep(x, length(categories)))
        )
        
      }
      
      categoriesdat <- rbind(categoriesdat, dat)
      
      A$i3 <- categoriesdat
      
      A$i2$Start[A$i2$ID == x][[1]] <- (input[[paste0("focusdateAreas", x)]])[1]
      
      if (input[[paste0("noend", x)]] == TRUE) {
        
        A$i2$Start[A$i2$ID == x][[1]] <- NA
        
      } else {
        
        A$i2$End[A$i2$ID == x][[1]] <- (input[[paste0("focusdateAreas", x)]])[2]
        
      }
      
      A$i2$Class[A$i2$ID == x][[1]] <- input[[paste0("class", x)]]
      
    }
    
    A$i3$ID <- as.list(1:nrow(A$i3))
    
    for (x in unlist(focusGroups)) {
      
      A$i3$Weight[(A$i3$Name == "Uncategorized") & (A$i3$ID == x)][[1]] <- NA
      
    }
    
  })
  
}