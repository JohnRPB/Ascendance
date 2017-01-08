#############################################################################################################################################################
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############------------------------------------------------ Setup Module 1: Focus Areas ------------------------------------------------------#############
#############-----------------------------------------------------------------------------------------------------------------------------------#############
#############################################################################################################################################################
#'
#' SUMMARY: 
#' 
#' User defines focus areas, weights them, defines categories, and sets some attributes of focus areas.
#'
#' MODULE MAP:
#' 
#' UI
#' 
#' (1)  ---- user presented with box to enter focus areas
#' (2)  ---- user enters information
#' (3)  ---- user presses 'submit' button
#' (4)  --------- pie chart created,  subdivided into equal areas representing focus area weights
#' (5)  --------- '+/-' buttons created below pie chart, letting user control subdivision areas (and therefore weights)
#' (6)  --------- input rows dynamically created for each focus area (in order from left to right...) 
#' (7)  ------------- a checkbox for user to indicate whether focus area is a class
#' (8)  ------------- a text input for user to write in categories
#' (9)  ------------- a date input for user to indicate start and end times
#' (10) ------------- a checkbox for user to indicate whehter they want to specify an end time or not
#' (11) ---- user enters information and (optional) plays around with '+/-' buttons to adjust focus area weights
#' (12) ---- user presses second 'submit' button
#' (13) ---- user presses 'Next'
#'
#' 
#' SERVER
#' 
#' (0)  ---- observers created for first and second submit buttons and pie chart.
#' (3)  ---- first three n-hierarchy levels generated, i0 and i1 with minimal information, i3 with user-defined focus areas
#'      ---- reactive expression run, creates observers for '+/-' buttons associated with focus areas that have not already had observers made for them
#' (11) ---- for every press of '+/-' buttons, all i2 weights redefined and stored
#' (12) ---- i3 created with info from step (8); all info from steps (7), (9), (10) saved to i2.
#' (13) ---- transition to next module
#' 
#' KEY COMPONENTS:
#' 
#' STORAGE
#' FIRST INPUT
#' DYNAMIC PIE CHART
#' SECOND INPUT
#' 
#' NOTES:
#' 
#' 

#' This is the tutorial module for creating focus areas and prioritizing them
#'

##################################################################################################################################################################
#############------------------------------------------------------------ User interface ------------------------------------------------------------#############
##################################################################################################################################################################

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

##################################################################################################################################################################
#############---------------------------------------------------------------- Server -----------------------------------------------------------------############
##################################################################################################################################################################

setup_focusAreas <- function(input, output, session) {
  
  #' =======================================================================| STORAGE |=======================================================================
  #'  
  #' Store reactive environment, dynamic variable 'y', and a reactive expression using 'y' to determine which observers to create
  
  rv <- reactiveValues()
  
  vals <- reactiveValues(y = "")
  
  #' Reactive expression generates observers for every '+/-' button, controlled for multiple focus area submissions.
  
  makeObsvs <- eventReactive(input$submitfocus, {
    
    #' ---------------------------------------| (VIABLE) OBSERVER IDS |-----------------------------------------
    #' STATE-CONTROL
    #'
    #' Generate list of observer IDs, excluding those for observers already created, to pass to lapply() in 
    #' next step.
    
    #' Create vector of only weighted focus area IDs (i.e. exclude 'Uncategorized')
    
    IDs <- rv$A$i2$ID[rv$A$i2$Name != "Uncategorized"] %>% unlist
    
    #' Generate boolean vector for IDs NOT in state-control variabl
    
    new_ind <- !(IDs %in% vals$y)
    
    #' Append list of new IDs to old list (excluding initial y = "") and store state
    
    vals$y <- append(IDs[new_ind], vals$y[vals$y != ""])
    
    #' ---------------------------------------| PLUS/MINUS OBSERVERS |-------------------------------------------
    #' STORAGE
    #'
    #' Two lists of observers created, one for the '+' and one for the '-' buttons.
    
    #' Generate first list and store
    #'
    #' ------------
    
    res1 <- lapply(IDs[new_ind], function (x) {
      
      #' Observer adds some to target focus area weight but controls sum of focus area weights to remain 
      #' equal to 1.
      
      observeEvent(input[[paste0("goplus_", x)]], {
        
        #' Add .01 to weight of focus Area with ID == x
        
        A$i2$Weight[A$i2$ID == x][[1]] <- A$i2$Weight[A$i2$ID == x][[1]] + .01 ## add .01 to focus group weight
        
        #' Subtract what was added to previous focus area to overall, but from all areas equally
        
        for (i in IDs[!(IDs %in% x)]) {
          
          A$i2$Weight[A$i2$ID == i][[1]] <- A$i2$Weight[A$i2$ID == i][[1]] - (.01)/(length(IDs)-1)
          
        }
        
        rv$Weights <- A$i2$Weight[A$i2$Name != "Uncategorized"] %>% unlist
        
      })
      
    })
    #' ------------
    #'  
    
    #' Generate second list and store
    #'
    #' ------------
    
    res2 <- lapply(IDs, function (x) {
    
      #' Observer subtracts some from target focus area weight but controls sum of focus area weights to remain 
      #' equal to 1.
      
      observeEvent(input[[paste0("gominus_", x)]], {
        
        #' Subtract .01 to weight of focus Area with ID == x
        
        A$i2$Weight[A$i2$ID == x][[1]] <- A$i2$Weight[A$i2$ID == x][[1]] - .01
        
        #' Add what was subtracted from previous focus area to overall, but from all areas equally
        
        for (i in IDs[!(IDs %in% x)]) {
          
          A$i2$Weight[A$i2$ID == i][[1]] <- A$i2$Weight[A$i2$ID == i][[1]] + (.01)/(length(IDs)-1)
          
        }
        
        rv$Weights <- A$i2$Weight[A$i2$Name != "Uncategorized"] %>% unlist
        
      })
      
    })
    #' ------------
    #' 
    
    #' Bind lists together
    
    res <- append(res1, res2)
    
    #' ---------------------------------------| PLUS/MINUS OBSERVERS |-------------------------------------------
    #' EXPRESSION
    #' 
    #' Express lists
    
    res
    
  })

  #' =====================================================================| FIRST INPUT |=====================================================================
  #' 
  #' Store reactive environment, dynamic variable 'y', and a reactive expression using 'y' to determine which observers to create
  
  observeEvent(eventExpr = input$submitfocus, handlerExpr = {
    
    ns <- session$ns #' namespacing
    
    #' Retreive submitted focus areas input, process to extract names, then add 'Uncategorized'
    
    focusAreas <- VectorizeString(input$selectfocus)
    focusAreas <- append(focusAreas, "Uncategorized")
    
    #' --------------------------------------| PRIMARY DATA STRUCTURES |------------------------------------------
    #' STORAGE
    #' 
    #' Creating four levels of n-hierarchy, bottom two storing focus areas and categories
    
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
    Names <- A$i2$Name[A$i2$Name != "Uncategorized"]

    #' -------------------------------------------------| UI |-----------------------------------------------------
    #' STORAGE
    #' 
    #' Define and store module functions that generate the user interface componenets that pop up when you hit 
    #' 'submit'
    
    #' Module function that creates ONE '+' and ONE '-' button, side by side
    
    prioritySet <- function(id) {
      
      column(6, column(3, h4(A$i2$Name[A$i2$ID == id])), 
             column(6, offset = 1, 
                    column(1, actionButton(ns(paste0("goplus_", id)), label = h4("+  "), width = "35px")),
                    column(1, offset = 1, actionButton(ns(paste0("gominus_", id)), label = h4("-  "), width = "35px"))
             )
      )
    }
    
    #' Module function that generates ONE row of inputs per focus area
    
    dRow_managePanel <- function(id, labels) {
      fluidRow(
        column(1, offset = 1, checkboxInput(ns(paste0("class", id)), label = "", value = FALSE, width = 30)),
        column(2, h3(paste0("    ", labels))),
        column(3, textInput(ns(paste0("focusAreas", id)), label = "", width = "350px")),
        column(4, dateRangeInput(ns(paste0("focusdateAreas", id)), label = "", width = "400px")),
        column(1, checkboxInput(ns(paste0("noend", id)), label = "", value = FALSE))
      )
    }
    
    #' -------------------------------------------------| UI |-----------------------------------------------------
    #' DYNAMIC
    #' 
    #' Generate ui and call stored module functions to produce dynamic display of '+/-' buttons and focus area 
    #' rows, as well as call dynamic pie-chart for weighing focus areas.
    
    output$ui <- renderUI({
      
      rows <- tagList()
      
      rows[2:(length(IDs)+1)] <- lapply(1:length(IDs), function(k) {
        
        dRow_managePanel(id = unlist(IDs)[k], labels = paste("  ", labels = unlist(Names))[k])
        
      })
      
      rows[[1]] <- fluidRow(column(1, offset = 1, h3("Class?")), column(3, offset = 2, h3("Categories")), column(3, h3("Dates active")), 
                            column(2, h3("  No end")))
      
      rows[[length(IDs)+2]] <- fluidRow(column(11, offset = 1, 
                                               h4("Enter typical contributive categories for your focus areas, such as 'Homework', 
                                                  'Test', and 'StudyTime' (if your focus areas is a class), or 'marketing'. Select 'No 
                                                   end' if you do not have an end date in mind."), 
                                               h4("Then hit 'submit' when you have carefully reviewed all this information; your 
                                                   settings will be stored in the app (although nothing will happen to incidate you 
                                                   clicked it). Press the 'Next' button to move on to the next part of the setup.")))
      
      rows[[length(IDs)+3]] <- fluidRow(br(), column(12, offset = 1, actionButton(ns("submitcat"), h4("Submit"), width = "150px")))
      
      columns <- tagList(column(1), "", column(6, rows))                                         
      
      columns[[2]] <- column(4, h3("Priority Settings"), box(width = 12, plotOutput(ns("pie"), height = 300), 
                                                             fluidRow(lapply(unlist(IDs), prioritySet))))
      
      columns
      
    })
    
    #'-------------------------------------------------------------------------------------------------------------
    #'

    #' Call reactive expression to generate observers for '+/-' buttons
    
    makeObsvs()
    
  })

  #' ==================================================================| DYNAMIC PIE CHART |==================================================================
  #' 
  #' Store reactive environment, dynamic variable 'y', and a reactive expression using 'y' to determine which observers to create
  
  #' Define function that outputs a chosen number of colors picked from a gradient
  
  colfunc <- colorRampPalette(c("slategray1", "deepskyblue4"))
  
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

  #' =====================================================================| SECOND INPUT |====================================================================
  #' 
  #' Generate 'i3' level of n-hierarchy and update 'i2' level with relevant info (e.g. is it a class or not, what are its start and end dates, etc)
  
  #' Set up observer to wait on second 'Submit' button.
  
  observeEvent(input$submitcat, {
    
    #' Grab IDs of user-generated i2 nodes
    
    i2UserNodeIDs <- A$i2$ID[A$i2$Name != "Uncategorized"] %>% unlist
    
    #' ------------------------------------------| BUILD i3 AND COMPLETE i2 |---------------------------------------------
    #' 
    #' Loop through each focus area's input row and grab its 'Class' boolean, its associated categories, its start and 
    #' end dates, and its 'End?' boolean. Use first, third, and fourth of these to complete i2 data frame; build i3 data
    #' frame with second.
    
    #' Initiate iterator with blank version of output
    
    i3 <- tibble(ID = list(), Name = list(), Notes = list(), End = list(), Start = list(), Weight = list(), 
                            i0 = list(), i1 = list(), i2 = list())
    
    #' Set input row/i2 node from which to grab inputs
    
    for (x in i2UserNodeIDs) {
      
      #' --------------------------------| i3 |-----------------------------------
      #' 
      #' Loop to build segment of i3 data frame
      
      #' Process input to extract category names.
      
      categories <- VectorizeString(input[[paste0("focusAreas", x)]]) #' Using GenFunc
      
      #' Add 'Uncategorized' category to current categories
      #'
      #' ------------
      
      #' If user submits no categories
      
      if (length(categories) == 0) {
        
        categories <- "Uncategorized"
        
      #' If user submits one or more categories

      } else {
        
        categories <- append(categories, "Uncategorized")        
        
      }
      #' ------------
      #'
      
      #' Generate rows for new categories
      
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
      
      #' Bind rows to prevous rows
      
      i3 <- rbind(i3, dat)
      
      #' --------------------------------| i2 |-----------------------------------
      #' 
      #' Loop to fill in columns of i2 data frame
      
      #' Start date
      
      A$i2$Start[A$i2$ID == x][[1]] <- (input[[paste0("focusdateAreas", x)]])[1]
      
      #' End date, controlling for value of 'No end?' boolean
      
      if (input[[paste0("noend", x)]] == TRUE) {
        
        A$i2$End[A$i2$ID == x][[1]] <- NA
        
      } else {
        
        A$i2$End[A$i2$ID == x][[1]] <- (input[[paste0("focusdateAreas", x)]])[2]
        
      }
      
      #' Class boolean
      
      A$i2$Class[A$i2$ID == x][[1]] <- input[[paste0("class", x)]]
      
    }
    #' ----------------------------------------------------------------------------------------------------------
    #'
    
    #' Generate IDs for i3
    
    i3$ID <- as.list(1:nrow(i3))
    
    #' Replace 'Uncategorized' i3 row weights with NAs
    
    for (x in unlist(i3$ID)) {
      
      i3$Weight[(i3$Name == "Uncategorized") & (i3$ID == x)][[1]] <- NA
      
    }
    
    #' Store i3 in AGE
    
    A$i3 <- i3
    
  })
  
}