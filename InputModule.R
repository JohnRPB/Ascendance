################################################################################################################################
#############------------------------------------------------------------------------------------------------------#############
#############------------------------------------ Ascendance Input Module -----------------------------------------#############
#############------------------------------------------------------------------------------------------------------#############
################################################################################################################################

#' This is the module that the user uses to summon the tasks he/she is contributing towards and inputs the time
#' data into. 

InputUI <- function(id) {
  
  ns <- NS(id)
  
  rows <- tagList()
  
  rows[[1]] <- fluidRow(
    column(11, offset = 1, h4("Select a task with the appropriate 
                              combination of focus area and category."))
  )
  rows[[2]] <- fluidRow(
    
    column(3, offset = 1, selectInput(ns("focusArea"), label = "Focus Area", choices = unlist(A$fdat$fa))),
    column(3, uiOutput(ns("categoryUI"))),
    column(3, uiOutput(ns("inputUI")))
    
  )
  rows[[3]] <- fluidRow(
    column(1, offset = 1, actionButton(ns("go"), label = "Log Time")),
    column(1,  actionButton(ns("Ntask"), label = "New Task")),
    column(1, actionButton(ns("accept"), label = "Accept"))
    
  )
  rows[[4]] <- uiOutput(ns("ui"))
  rows[[5]] <- br()
  
  tags$div(id = ns("include"),  rows )
  
  
}

Input <-function(input, output, session, Path) {
  
  load(Path)
  
  ns <- session$ns
  
  rv <- reactiveValues()
  
  output$categoryUI <- renderUI({
    
    selectInput(ns("category"), 
                label = "Category", 
                choices = unlist(A$cdat$cat[A$cdat$fa == input[["focusArea"]] ]))
    
  })
  
  output$inputUI <- renderUI({
    
    ch <- unlist(A$tdat$tasks[(A$tdat$fa == input[["focusArea"]]) & (A$tdat$cat == input[["category"]])], use.names = FALSE)
    
    selectInput(ns("task"), label = "Task", choices = ch)
    
  })
  
  add <- reactiveValues(x = 1, y = "")
  
  observeEvent(input$go, priority = 3, {
    
    a <- input[["focusArea"]]; b <- input[["category"]]; c <- input[["task"]]
    add$y <- as.character(A$tdat$ID[(A$tdat$fa == a) & (A$tdat$cat == b) & (A$tdat$tasks == c)][[1]])
    
  })
  
  makeObs <- eventReactive(add$x, {
    
    obsnum <- add$x; ID <- add$y
    
    pressed <- reactiveValues()
    pressed[[paste0("val_", obsnum)]] <- 1
    
    observeEvent(input[[paste0("record_", obsnum)]], {
      
      timediff <- A$tentr$end - A$tentr$start
      
      now <- Sys.time()
      
      if ( (input[[paste0("start_", obsnum)]] == "") & (input[[paste0("end_", obsnum)]] == "") ) {
        
        if (pressed[[paste0("val_", obsnum)]] %% 2 != 0) {
            
            newstart <- A$tentr$start[1,]; newstart[] <- lapply(1:ncol(newstart), function(x) { newstart[,x] <- now })
            newend <- A$tentr$end[1,]; newend[] <- lapply(1:ncol(newend), function(x) { newend[,x] <- now })
            neweff <- A$tentr$eff[1,]; neweff[] <- lapply(1:ncol(neweff), function(x) { neweff[,x] <- 0})
            
            A$tentr$start <- rbind(A$tentr$start, newstart)
            A$tentr$end <- rbind(A$tentr$end, newend)
            A$tentr$eff <- rbind(A$tentr$eff, neweff)
            
            A$tentr$start[nrow(A$tentr$start), ID] <- now
            A$tentr$end[nrow(A$tentr$end), ID] <- now
            
            pressed[[paste0("val_", obsnum)]] <- pressed[[paste0("val_", obsnum)]] +1
          
        } else {
          
          A$tentr$end[nrow(A$tentr$end), ID] <- now
          A$tentr$eff[nrow(A$tentr$eff), ID] <- input[[paste0("slider_", obsnum)]]
          
          pressed[[paste0("val_", obsnum)]] <- pressed[[paste0("val_", obsnum)]] +1
          
        }
        
      } else {
          
          start <- as.POSIXct(strptime(input[[paste0("start_", obsnum)]], "%Y-%m-%d %H:%M:%S"))
          end <-  as.POSIXct(strptime(input[[paste0("end_", obsnum)]], "%Y-%m-%d %H:%M:%S"))
          
          if (is.na(start) | is.na(end)) {
            
            stopApp("Error: Time format must be 'yyyy-mm-dd hh:mm:ss', military time.")
            
          } else if (start < as.POSIXct(A$fps[[1]][1]) | end < as.POSIXct(A$fps[[1]][1])) {
            
            stopApp("Error: Cannot submit time(s) prior to start of first focus period.")
            
          } else {
            
            newstart <- A$tentr$start[1,]; newstart[] <- lapply(1:ncol(newstart), function(x) { newstart[,x] <- now })
            newend <- A$tentr$end[1,]; newend[] <- lapply(1:ncol(newend), function(x) { newend[,x] <- now })
            neweff <- A$tentr$eff[1,]; neweff[] <- lapply(1:ncol(neweff), function(x) { neweff[,x] <- 0})
            
            A$tentr$start <- rbind(A$tentr$start, newstart)
            A$tentr$end <- rbind(A$tentr$end, newend)
            A$tentr$eff <- rbind(A$tentr$eff, neweff)
            
            A$tentr$start[nrow(A$tentr$start), ID] <- start
            A$tentr$end[nrow(A$tentr$end), ID] <- end
            A$tentr$eff[nrow(A$tentr$eff), ID] <- input[[paste0("slider_", obsnum)]]
            
          }
        
      }
      
      save(A, file = Path)
      
    })
    
  })
  
  observeEvent(input$go, priority = 3, { makeObs() })
  
  observeEvent(input$go, priority = 3, {
    
    obsnum <- add$x
    
    insertUI(
      selector = paste0("#", ns("include")),
      where = "afterEnd",
      
      ui = {
        
        cols <- tagList()
        cols[[1]] <- column(5, textInput(ns(paste0("start_", add$x)), label = "Start"))
        cols[[2]] <- column(5, textInput(ns(paste0("end_", add$x)), label = "End"))
        Row <- tagList()
        Row[[1]] <- fluidRow(column(5, h4(paste0(input[["task"]], " ", "(", add$x, ")"))))
        Row[[2]] <- do.call(fluidRow, cols)
        taskRow <- fluidRow(column(5, Row), 
                            column(1, checkboxInput(ns(paste0("complete_", add$x)), label = "Completed?")),
                            column(5, textInput(ns(paste0("note_", add$x)), label = "Note"))
        )
        fluidRow(column(9, offset =1, wellPanel(taskRow, fluidRow(
          
          column(9, sliderInput(ns(paste0("slider_", add$x)), label = "Estimated Efficiency", 
                                min = 0, max = 1, value = .5, step = .01)),
          
          column(1, actionButton(ns(paste0("record_", add$x)), color = "darkgrey", label = h4("Record")))
          
        )
        
        )))
        
      }
      
    )
    
    add$x <- obsnum + 1
    
  })
  
  inserted <- reactiveValues(tasknum = 1)
  
  observeEvent(input$Ntask, priority = 3, {
    
    insertUI(
      selector = paste0("#", ns("include")),
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
        
        rows <- tagList()
        rows[[1]] <- fluidRow(cols)
        rows[[2]] <- fluidRow(column(4, offset = 1, dateInput(ns(paste0("start", inserted$tasknum)), 
                                                              label = "Start")),
                              column(4, dateInput(ns(paste0("end", inserted$tasknum)), label = "End")))
        
        rows
        
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
  
  observeEvent(input$Ntask, priority = 1, {inserted$tasknum <- inserted$tasknum + 1})
  
  observeEvent(input$accept, priority = 3, {
    
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
    inputsStart <- lapply(1:num, function(x) {as.POSIXct(input[[paste0("start", x)]], origin = "1970-01-01 00:00.00 UTC")})
    inputsEnd <- lapply(1:num, function(x) {as.POSIXct(input[[paste0("end", x)]], origin = "1970-01-01 00:00.00 UTC")})
    
    ntdat <- data.frame(cbind(tasks = inputsT, ID = as.list((1000 + nrow(A$tdat)):(1000 + num - 1 + nrow(A$tdat))), 
                              fa = inputsF, cat = inputsC, est = inputsE, 
                               start = inputsStart, end = inputsEnd))
    
    A$tdat <- rbind(A$tdat, ntdat)
    
    ## fixing fdat and tdat data frames to accomodate new tasks
    
    # finding the indexes for unique focus areas, in the tdat data frame
    
    for (i in A$tdat$fa[!(duplicated(A$tdat$fa))]) {
      
      # fixing fdat's task list for given focus areas
      
      A$fdat$tasks[A$fdat$fa == i][[1]] <- unlist(A$tdat$tasks[A$tdat$fa == i])
      
      # finding the index for categories of given focus areas
      
      for (x in A$fdat$cat[A$fdat$fa == i][[1]]) {
        
        # avoiding non-existent tasks for given combination of focus area and category
        
        if (unlist(A$tdat$tasks[(A$tdat$cat == x) & (A$tdat$fa == i)]) %>% length == 0) {
          
        } else {
          
          # fixing cdat's task list for given combination of category and focus area
          
          A$cdat$tasks[(A$cdat$cat == x) & (A$cdat$fa == i)][[1]] <- unlist(A$tdat$tasks[(A$tdat$cat == x) & (A$tdat$fa == i)])
        
          }
      }
      
    }
    
    ## adding new task to tentr by ID
    
    taskIDs <- as.character(A$tdat$ID[!(A$tdat$ID %in% colnames(A$tentr$start))])
    
    dum <- A$tentr$end - A$tentr$start; Logged <- A$tentr$end - A$tentr$start
    
    dum[] <- lapply(1:ncol(dum), function(x) {
      
      lapply(1:nrow(dum), function(y) {
        
        dum[y,x] <- dum[y,x] != as.difftime(0, units = "hours")   
        
      })
      
    })
    
    dates <- append(as.POSIXct(A$fps[[1]][1], origin = "1970-01-01 00:00.00 UTC") + 
                   as.difftime(7, units = "hours"), as.POSIXct(A$tentr$start[dum == FALSE], 
                                                               origin = "1970-01-01 00:00.00 UTC"))
    duplicates <- dates[duplicated(dates)]
    dates <- unique(duplicates)
    
    newcol <- A$tentr$start[,1, drop = FALSE]
    newcol[] <- dates
    
    for(i in taskIDs) {
      
      names(newcol) <- i
      
      A$tentr$start <- cbind(A$tentr$start, newcol)
      A$tentr$end <- cbind(A$tentr$end, newcol)
      
      newcol[] <- rep(0, nrow(newcol))
      A$tentr$eff <- cbind(A$tentr$eff, newcol)
      
      A$tentr$auto <- cbind(A$tentr$auto, data.frame(temp = rep("", nrow(A$tentr$auto))))
      colnames(A$tentr$auto)[ncol(A$tentr$auto)] <- i
     
      A$Logged <- cbind(A$Logged, data.frame(temp = rep(0, nrow(A$Logged))))
      colnames(A$Logged)[ncol(A$Logged)] <- i
      
    }
    
    ###### Experimental insertion of previous code to build opC and opCs
    
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
    
    ######
    
    save(A, file = Path)
    
  })
  
  
}

