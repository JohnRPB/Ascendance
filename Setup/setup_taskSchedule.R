################################################################################################################################
#############------------------------------------------------------------------------------------------------------#############
#############---------------------------------- Tutorial Module 4: Task Scheulde ----------------------------------#############
#############------------------------------------------------------------------------------------------------------#############
################################################################################################################################

#' This module will allow you to play with the absolute start and end times for tasks to arrive at a word distribution
#' that is comfortable for you. 
#' 
#' PROTOCOL FOR THE SETUP PROCESS:
#' 
#' Having reached this module, and stopped the app (as instructed by the writing within it), you will need to do the 
#' following things, in order to run it again such that it will record your adjustments:
#' 
#' (1) Go the main app script file and find the conditional , 'file.exists(path) == TRUE'. Set 'TRUE' to 'FALSE'.
#' (2) Find the line, 'A <<- new.env()'. Comment out that line and uncomment the line below it.
#' (3) Find the line, 'tut$stage <- 1'. Change '1' to '4'. Save the app script file.
#' (4) Hit the 'Run App' button on the main app script file and use the module as normally; play with the sliders 
#' until you are satisfied with your result (you may still encounter the fixed first two days problem.) Then hit 
#' 'Next'. The app will stop. THEN COME BACK AND READ THE REST OF THIS!
#' (5) Go back to the main app script file and change the parts of script specified in steps (1)-(2) back to the way
#' they were. Hit the 'Run App' button again. You are now done with the setup process and can use the main app.
#' 
#' NOTE: Use the 'Input' module carefully; only click 'Log Time' if a task name appears in the user interface 
#' component labeled 'Task'. Do not try to log time for a task and add a new task in the same run of the app. When
#' any kind of change has been made to your data, the safest route is to stop the app and then run it again, to see
#' the changes. 
#' 
#' Right now, Ascendance is little more advanced than the original TimeProg prototype; the only thing it does is 
#' reliably log the time you've spent in pursuit of your goals, and lets you add new tasks. You can see individual 
#' tasks by stopping the app and typing 'A$tdat' in the command line.
#' 
#' It is probably best to use Ascendance with a companion sheet of paper; keep a record of the start and end times
#' for your work sessions and the tasks they belong to, then log that information in Ascendance. Note manually when
#' the task is completed (Ascendance will later be able to incorporate this information and adjust the optimal curve 
#' accordingly, but right now it is primariliy a time-logger, only *very* roughly an accountability tool). You will
#' need to keep track of individual task completion by the usual methods you employ; right now, you can only see your
#' progression for different focus areas, and overall. That is, Ascendance only helps you get an idea of the general
#' amount of effort you are putting into your focus areas and overall workload.
#' 

cumtime <- function(x) {
  
  for (i in 1:(length(x)-1)) {
    
    x[i+1] <- x[i] + x[i+1] 
    
  }
  
  x
  
}

setup_taskScheduleUI <- function(id) {
  
  ns <- NS(id)
  
  len <- length(A$tdat$tasks)
  
  IDs <- A$tdat$ID %>% unlist %>% as.character
  
  IDs <- lapply(IDs, function(n) {
    
    i <- A$tdat$tasks[A$tdat$ID == n][[1]]
    j <- A$tdat$cat[A$tdat$ID == n][[1]]
    k <- A$tdat$fa[A$tdat$ID == n][[1]]
    
      paste(k, j, i, sep = "-")
  }) %>% unlist
  
  makeSlider <- function(x) {
    
    box(width = 3, sliderInput(inputId = ns(paste0("slider_", x)), 
                               label = x, 
                               value = as.POSIXct(A$fps[[1]]) + as.difftime(7, units = "hours"),
                               min = as.POSIXct(A$fps[[1]][1]) + as.difftime(7, units = "hours"),
                               max = as.POSIXct(A$fps[[1]][2]) + as.difftime(7, units = "hours"))
        )
    
  }
  
  rows <- tagList()
  rows[[1]] <- fluidRow(
    column(10, offset = 1, 
           h2({"Step 4: Planning the focus period"}),
           h4({"The last thing that remains to be done before Ascendance is set up on your computer is deciding 
             how to split up the estimated hours you assigned to your tasks. Ascendance makes this very simple;
             just adjust the sliders beneath each of the tasks until you arrive at a work distribution that works 
             for you. When you would like to start, just press 'Go'. (Note that you will need to follow the protocol 
             laid out at the beginning of this module's script file in order to have this module record any of your 
             data. This includes stopping the app now.)"})
    )
  )
  rows[[2]] <- br()
  rows[[3]] <- fluidRow(column(10, offset = 1, lapply(IDs, function(x) {makeSlider(x)})))
  rows[[4]] <- fluidRow(column(10, offset = 1, plotOutput(ns("workDist"))))
  rows
  
}

setup_taskSchedule <- function(input, output, session, proceed, Path) {
  
  ns <- session$ns
  
  ############ ------------- Create observers to update start and end times for tasks ------------- ############
  
  IDs <- A$tdat$ID %>% unlist %>% as.character
  
  lapply(IDs, function(n) {
    
    i <- A$tdat$tasks[A$tdat$ID == n][[1]]
    j <- A$tdat$cat[A$tdat$ID == n][[1]]
    k <- A$tdat$fa[A$tdat$ID == n][[1]]
    
    observeEvent(input[[paste0("slider_", paste(k, j, i, sep = "-"))]], priority = 2, {
      
      A$tdat$start[A$tdat$ID == n] <- (input[[paste0("slider_", paste(k, j, i, sep = "-"))]])[1]
      A$tdat$end[A$tdat$ID == n] <- (input[[paste0("slider_", paste(k, j, i, sep = "-"))]])[2]
      
    })
    
  })
  
  ############ ------------ Laborious process of building optimal curve for data frame ------------ ############

  #' Setting up single observer to react to any one of the sliders and update its graph accordingly
    
  observeEvent({
    
    IDs <- A$tdat$ID %>% unlist %>% as.character
    
    lapply(IDs, function(n) {
      
      i <- A$tdat$tasks[A$tdat$ID == n][[1]]
      j <- A$tdat$cat[A$tdat$ID == n][[1]]
      k <- A$tdat$fa[A$tdat$ID == n][[1]]
      
      input[[paste0("slider_", paste(k, j, i, sep = "-"))]]
      
    })
    
  }, priority = 1, {
    
    a <- A$tdat$start %>% unlist %>% as.POSIXct(., origin = "1970-01-01 00:00.00 UTC") + as.difftime(7, units = "hours")
    b <- A$tdat$end %>% unlist %>% as.POSIXct(., origin = "1970-01-01 00:00.00 UTC") + as.difftime(7, units = "hours")
    
    fp <- seq(as.POSIXct(A$fps[[1]][1], origin = "1970-01-01 00:00.00 UTC") + as.difftime(7, units = "hours"), 
              as.POSIXct(A$fps[[1]][2], origin = "1970-01-01 00:00.00 UTC") + as.difftime(7, units = "hours"), by = "days")
    
    taskperiods <- lapply(seq(length(a)), function(x) {
      
      seq(from = as.POSIXct(as.Date(floor_date(a[x], "day"), origin = "1970-01-01 00:00.00 UTC")) + as.difftime(7, units = "hours"), 
          to =  as.POSIXct(as.Date(ceiling_date(b[x], "day"), origin = "1970-01-01 00:00.00 UTC")) + as.difftime(7, units = "hours"), by = "days")
      
    })
    
    periodsIndex <- lapply(seq(length(a)), function(x) {
      
      fp %in% taskperiods[[x]]
      
    })
    
    opCs <- lapply(seq(length(a)), function(x) {
      
      est <- A$tdat$est[[x]]
      units(est) <- "hours"
      
      a <- as.POSIXct(as.Date(floor_date(a[x], "day"), origin = "1970-01-01 00:00.00 UTC")) + as.difftime(7, units = "hours")
      b <- as.POSIXct(as.Date(ceiling_date(b[x], "day"), origin = "1970-01-01 00:00.00 UTC")) + as.difftime(7, units = "hours")
      
      tasklength <- as.numeric(difftime(b, a))
      
      slope <- est/tasklength
      
      optC <- slope*rep(1, length(fp))
      optC[periodsIndex[[x]] == FALSE] <- 0
      optC
      
    })
    
    names(opCs) <- as.character(unlist(A$tdat$ID))
    
    opCs <- data.frame(opCs)
    
    names(opCs) <- as.character(unlist(A$tdat$ID))
    
    opCs[] <- lapply(1:length(opCs), function(x) {
      
      as.numeric(opCs[,x])
      
    })
    
    taskIDs <- lapply(A$cdat$ID, function(i) {
      
      a <- A$cdat$cat[A$cdat$ID == i][[1]]; b <- A$cdat$fa[A$cdat$ID == i][[1]]
      
      unlist(A$tdat$ID[(A$tdat$cat == a) & (A$tdat$fa == b)])
      
    })
    
    names(taskIDs) <- as.character(A$cdat$ID)
    
    for (i in length(taskIDs):1) {
      
      if (length(taskIDs[[i]]) > 1) {
        
        optC <- rowSums(opCs[,as.character(taskIDs[[i]])])
        
      } else if (length(taskIDs[[i]]) == 0) {
        
        optC <- rep(0, nrow(opCs))
        
      } else {
        
        optC <- opCs[,as.character(taskIDs[[i]])]
        
      }
      
      opCs <- cbind(I(as.list(optC)), opCs)
      
      colnames(opCs)[1] <- names(taskIDs[i])
      
    }
    
    for (i in unlist(A$fdat$fa)) {
      
      index <- unlist(A$tdat$ID[A$tdat$fa == i])
      
      if (length(index) > 1) {
        
        
        optC <- rowSums(opCs[,as.character(index)])
        
      } else if (length(index) == 0) {
        
        optC <- rep(0, nrow(opCs))
        
      } else {
        
        optC <- opCs[,as.character(index)]
        
      }
      
      opCs <- cbind(I(as.list(optC)), opCs)
      
      colnames(opCs)[1] <- i
      
    }
    
    opCs <- cbind(full =  I(as.list(rowSums(opCs[,colnames(opCs) %in% A$tdat$ID]))), opCs)
    
    opCs[] <- lapply(1:ncol(opCs), function(x) {
      
      as.difftime(unlist(opCs[,x]), units = "hours")
      
    })
    
    opC <- opCs
    
    opC[] <- lapply(1:ncol(opC), function(x) {
      
      cumtime(unlist(opCs[,x]))
      
    })
    
    opCs <- cbind(fp = fp, opCs)
    opC <- cbind(fp = fp, opC)
    
    x <- c(opC$fp[1], opC$fp[1], opC$fp[opC$fp > opC$fp[1]], opC$fp[opC$fp == max(opC$fp)], opC$fp[1])
    
    y <- c(as.difftime(0, units = "hours"), opC$full[opC$fp == opC$fp[1]], opC$full[opC$fp > opC$fp[1]],
           as.difftime(0, units = "hours"), as.difftime(0, units = "hours"))
    
    shade <- data.frame(x, y)
    
    A$opCs <- opCs
    A$opC <- opC
    
    tentr <- as.list(rep(Sys.time(), length(A$tdat$ID)))
    names(tentr) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    tentr <- data.frame(tentr, stringsAsFactors = FALSE)
    colnames(tentr) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    
    tentr <- lapply(1:2, function(x) { tentr })
    
    names(tentr) <- c("start", "end")
    
    auto <- as.list(rep("", length(A$tdat$tasks)))
    names(auto) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    auto <- data.frame(auto, stringsAsFactors = FALSE)
    colnames(auto) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    
    eff <- as.list(rep(0, length(A$tdat$tasks)))
    names(eff) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    eff <- data.frame(eff, stringsAsFactors = FALSE)
    colnames(eff) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    
    dummy <- list()
    
    dummy$auto <- auto; dummy$start <- tentr$start; dummy$end <- tentr$end; dummy$eff <- eff
    
    A$tentr <- dummy
    
    output$workDist <- renderPlot({
      
      ggplot(data = A$opCs, aes(x = fp, y = full)) + geom_bar(stat = "identity") + ggtitle("Workload per day") + 
        theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
        scale_x_datetime(breaks = A$opCs$fp, labels = format(A$opCs$fp,  "%a %d"), name = NULL) + 
        scale_y_continuous(name = NULL)
      
    })
    
    save(A, file = Path)
    
  })

  ############ ------------------------------------------------------------------------------------ ############
  
}